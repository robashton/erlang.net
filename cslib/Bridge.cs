using System;
using CsLib.Erlang;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Linq;
using System.IO;

namespace CsLib
{
  [StructLayout(LayoutKind.Sequential)]
  public unsafe struct CreateArgs
  {
    public IntPtr handle;
    public delegate* <IntPtr, ErlNifTerm> @return;
    public delegate* <ErlNifEnv, IntPtr, IntPtr, IntPtr, ErlNifTerm> load_assembly;
    public delegate* <ErlNifEnv, IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm> erlang_callback;
  }

  public class Bridge {

    Runtime runtime;
    IApp running_app;

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

      Runtime runtime = new Runtime();
      Bridge instance = new Bridge(runtime);

      // If you Console.WriteLine before this, you'll break the erlang shell
      Console.SetOut(new PrintFWriter(runtime));

      var handle = GCHandle.Alloc(instance);
      args->handle = GCHandle.ToIntPtr(handle);
      args->@return = &Return;
      args->load_assembly = &LoadAssemblyWrapper;
      args->erlang_callback = &ErlangCallbackWrapper;
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

    public static ErlNifTerm Return(IntPtr hptr) {
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
        // TODO: Ask stears what he remembers about app domains and this shit..
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

        return term;
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
