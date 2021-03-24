using System;
using CsLib.Erlang;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Reflection;
using System.Linq;
using System.IO;

using E = CsLib.Erlang.Erlang;

namespace CsLib
{
  public class Bridge {
    
    [StructLayout(LayoutKind.Sequential)]
    private unsafe struct CreateArgs
    {
      public IntPtr handle;
      public delegate* <IntPtr, ErlNifTerm> @return;
      public delegate* <ErlNifEnv, IntPtr, IntPtr, IntPtr, ErlNifTerm, ErlNifTerm> load_assembly;
      public delegate* <ErlNifEnv, IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm> erlang_callback;
    }

    private delegate ErlNifTerm ReturnDelegate(IntPtr handle);
    private delegate ErlNifTerm LoadAssemblyDelegate(ErlNifEnv env, IntPtr bridge, IntPtr assemblyName, IntPtr bridgeName, ErlNifTerm args);
    private delegate ErlNifTerm ErlangCallbackDelegate(ErlNifEnv nev, IntPtr bridge, ErlNifTerm callback, ErlNifTerm args);

    Object runningApp;

    ReturnDelegate @return;
    LoadAssemblyDelegate load_assembly;
    ErlangCallbackDelegate erlang_callback;
    
    private Bridge() {
    }

    public class PrintFWriter : StringWriter
    {
      public PrintFWriter()
      {
      }

      public override void WriteLine(string value)
      {
        E.WriteDebug(value);
      }
    }

    unsafe public static int Create(IntPtr ptr, int argLength) {

      CreateArgs* args = (CreateArgs*)ptr;

      Bridge instance = new ();

      // If we don't stash the delegates to our static functions on the gcroot, they get cleared up before we're done (lol)
      // and if we try and use function pointers to static functions with 'unmanagedcallersonly', we get 
      // PAL_SEHException, so this long-winded method is all I can really be bothered with because I'm not 
      // downloading the .NET core symbols and fighting the use of them on nixos just to get to the bottom of it
      instance.@return = Return;
      instance.load_assembly = LoadAssemblyWrapper;
      instance.erlang_callback = ErlangCallbackWrapper;

      // If you Console.WriteLine before this, you'll break the erlang shell
      Console.SetOut(new PrintFWriter());

      var handle = GCHandle.Alloc(instance);
      args->handle = GCHandle.ToIntPtr(handle);
      args->@return = (delegate* <IntPtr, ErlNifTerm>)Marshal.GetFunctionPointerForDelegate(instance.@return);
      args->load_assembly = (delegate* <ErlNifEnv, IntPtr, IntPtr, IntPtr, ErlNifTerm, ErlNifTerm>) Marshal.GetFunctionPointerForDelegate(instance.load_assembly);
      args->erlang_callback = (delegate* <ErlNifEnv, IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm>) Marshal.GetFunctionPointerForDelegate(instance.erlang_callback);

      return 0;
    }

    static ErlNifTerm LoadAssemblyWrapper(ErlNifEnv env, IntPtr bridge, IntPtr assemblyName, IntPtr typeName, ErlNifTerm args) {
      var res = ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).LoadAssembly(env, Marshal.PtrToStringAuto(assemblyName), Marshal.PtrToStringAuto(typeName), args);
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
      return E.WithEnv(env, () => {
        ErlangCallback callback = (ErlangCallback)E.GetObjectReference(fn);
        return callback(args);
     });
    }

    public ErlNifTerm LoadAssembly(ErlNifEnv env, String filepath, String typeName, ErlNifTerm args)
    {
      return E.WithEnv(env, () => {
        AssemblyName assemblyName = AssemblyName.GetAssemblyName(filepath);
        Assembly assembly = Assembly.Load(assemblyName);

        var appType = assembly.GetExportedTypes()
              .Where(t => t.FullName == typeName)
              .FirstOrDefault();

        if (appType == null) {
          return E.MakeTuple2(E.MakeAtom("error"), E.MakeAtom("enoent"));
        }

        var withArgs = appType.GetInterfaces()
                            .Where(x => x.IsGenericType && x.GetGenericTypeDefinition() == (typeof(IApp<>)))
                            .FirstOrDefault();

        var withVoid = appType.GetInterfaces()
                            .Where(x => x.Name == typeof(IApp).Name)
                            .FirstOrDefault();

        if(withVoid == null && withArgs == null) {
          return E.MakeTuple2(E.MakeAtom("error"), E.MakeAtom("interface_missing"));
        }

        var ctor = appType.GetConstructor(Type.EmptyTypes);
        this.runningApp = ctor.Invoke(new object[]{});

        if(withVoid != null) {
           var term = ((IApp)this.runningApp).Start();
           return E.MakeTuple2(E.MakeAtom("ok"), E.ExportAuto(term));

        } else {
          var argType = withArgs.GetGenericArguments()[0];
          var method = withArgs.GetMethod("Start");
          var input = E.Coerce(args, argType);
          
          var term = method.Invoke(this.runningApp, new object[] { input });
          return E.MakeTuple2(E.MakeAtom("ok"), E.ExportAuto(term));
        }
      });
    }
  }
}
