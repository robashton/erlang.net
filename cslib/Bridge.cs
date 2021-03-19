using System;
using CsLib.Erlang;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Reflection;
using System.Linq;
using System.IO;

namespace CsLib
{
  public class Bridge {
    
    [StructLayout(LayoutKind.Sequential)]
    private unsafe struct CreateArgs
    {
      public IntPtr handle;
      public delegate* <IntPtr, ErlNifTerm> @return;
      public delegate* <ErlNifEnv, IntPtr, IntPtr, IntPtr, ErlNifTerm> load_assembly;
      public delegate* <ErlNifEnv, IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm> erlang_callback;
    }

    private delegate ErlNifTerm ReturnDelegate(IntPtr handle);
    private delegate ErlNifTerm LoadAssemblyDelegate(ErlNifEnv env, IntPtr bridge, IntPtr assemblyName, IntPtr bridgeName);
    private delegate ErlNifTerm ErlangCallbackDelegate(ErlNifEnv nev, IntPtr bridge, ErlNifTerm callback, ErlNifTerm args);

    Runtime runtime;
    IApp running_app;

    ReturnDelegate @return;
    LoadAssemblyDelegate load_assembly;
    ErlangCallbackDelegate erlang_callback;
    
    private Bridge(Runtime runtime) {
      this.runtime = runtime;
    }

    public class PrintFWriter : StringWriter
    {
      Runtime runtime;
      public PrintFWriter(Runtime runtime)
      {
        this.runtime = runtime;
      }

      public override void WriteLine(string value)
      {
        this.runtime.WriteDebug(value);
      }
    }

    unsafe public static int Create(IntPtr ptr, int argLength) {

      CreateArgs* args = (CreateArgs*)ptr;

      Runtime runtime = new ();
      Bridge instance = new (runtime);

      // If we don't stash the delegates to our static functions on the gcroot, they get cleared up before we're done (lol)
      // and if we try and use function pointers to static functions with 'unmanagedcallersonly', we get 
      // PAL_SEHException, so this long-winded method is all I can really be bothered with because I'm not 
      // downloading the .NET core symbols and fighting the use of them on nixos just to get to the bottom of it
      instance.@return = Return;
      instance.load_assembly = LoadAssemblyWrapper;
      instance.erlang_callback = ErlangCallbackWrapper;

      // If you Console.WriteLine before this, you'll break the erlang shell
      Console.SetOut(new PrintFWriter(runtime));

      var handle = GCHandle.Alloc(instance);
      args->handle = GCHandle.ToIntPtr(handle);
      args->@return = (delegate* <IntPtr, ErlNifTerm>)Marshal.GetFunctionPointerForDelegate(instance.@return);
      args->load_assembly = (delegate* <ErlNifEnv, IntPtr, IntPtr, IntPtr, ErlNifTerm>) Marshal.GetFunctionPointerForDelegate(instance.load_assembly);
      args->erlang_callback = (delegate* <ErlNifEnv, IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm>) Marshal.GetFunctionPointerForDelegate(instance.erlang_callback);

      return 0;
    }

    static ErlNifTerm LoadAssemblyWrapper(ErlNifEnv env, IntPtr bridge, IntPtr assemblyName, IntPtr typeName) {
      var res = ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).LoadAssembly(env, Marshal.PtrToStringAuto(assemblyName), Marshal.PtrToStringAuto(typeName));
      return res;
    }
    
    static ErlNifTerm ErlangCallbackWrapper(ErlNifEnv env, IntPtr bridge, ErlNifTerm fn, ErlNifTerm args) {
      var res = ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).ErlangCallback(env, fn, args);
      return res;
    }

    internal static ErlNifTerm Return(IntPtr hptr) {
      GCHandle.FromIntPtr(hptr).Free();
      return ErlNifTerm.Zero;
    }

    public ErlNifTerm ErlangCallback(ErlNifEnv env, ErlNifTerm fn, ErlNifTerm args)
    {
      this.runtime.SetEnv(env);
      ErlangCallback callback = (ErlangCallback)this.runtime.GetObjectReference(fn);
      return callback(runtime, args);
    }

    public ErlNifTerm LoadAssembly(ErlNifEnv env, String filepath, String typeName)
    {
      this.runtime.SetEnv(env);

      try
      {
        AssemblyName assemblyName = AssemblyName.GetAssemblyName(filepath);
        Assembly assembly = Assembly.Load(assemblyName);


        var appType = assembly.GetExportedTypes()
          .FirstOrDefault(t => t.GetInterface(typeof(IApp).Name) != null && t.FullName == typeName);

        if (appType == null) {
          return this.runtime.MakeAtom("enoent");
        }

        var ctor = appType.GetConstructor(Type.EmptyTypes);

        this.running_app = (IApp)ctor.Invoke(new object[]{});

        var term = this.running_app.Start(this.runtime);

        return this.runtime.MakeTuple2(this.runtime.MakeAtom("ok"), this.runtime.ExportAuto(term));
      }
      // I think I just need to do this or we're going to end up with exceptions bubbling up into C
      // and we'll end up leaking shit all over the place
      catch (Exception ex) {
        Console.WriteLine("Exception in bridge: " + ex.ToString());
        return this.runtime.MakeAtom("error");
      }
    }
  }
}
