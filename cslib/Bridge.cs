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
    public IntPtr runtime;
    public delegate* <IntPtr, ErlNifTerm> @return;
    public delegate* <ErlNifEnv, IntPtr, IntPtr, IntPtr, ErlNifTerm> load_assembly;
    public delegate* <ErlNifEnv, IntPtr, IntPtr, ErlNifTerm> process_init;
    public delegate* <ErlNifEnv, IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm> process_msg;
    public delegate* <ErlNifEnv, IntPtr, ErlNifTerm, ErlNifTerm> process_timeout;
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

      Runtime runtime = new Runtime(args->runtime);
      Bridge instance = new Bridge(runtime);

      // If you Console.WriteLine before this, you'll break the erlang shell
      Console.SetOut(new PrintFWriter(runtime));

      var handle = GCHandle.Alloc(instance);
      args->handle = GCHandle.ToIntPtr(handle);
      args->@return = &Return;
      args->load_assembly = &LoadAssemblyWrapper;
      args->process_init = &ProcessInitWrapper;
      args->process_msg = &ProcessMsgWrapper;
      args->process_timeout = &ProcessTimeoutWrapper;

      return 0;
    }

    static ErlNifTerm LoadAssemblyWrapper(ErlNifEnv env, IntPtr bridge, IntPtr assemblyName, IntPtr typeName) {
      var res = ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).LoadAssembly(env, Marshal.PtrToStringAuto(assemblyName), Marshal.PtrToStringAuto(typeName));
      return res;
    }

    static ErlNifTerm ProcessInitWrapper(ErlNifEnv env, IntPtr bridge, IntPtr fn) {
      var res = ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).ProcessInit(env, fn);
      return res;
    }

    static ErlNifTerm ProcessMsgWrapper(ErlNifEnv env, IntPtr bridge, ErlNifTerm fn, ErlNifTerm msg) {
      return ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).ProcessMsg(env, fn, msg);
    }

    static ErlNifTerm ProcessTimeoutWrapper(ErlNifEnv env, IntPtr bridge, ErlNifTerm fn) {
      return ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).ProcessTimeout(env, fn);
    }

    public static ErlNifTerm Return(IntPtr hptr) {
      GCHandle.FromIntPtr(hptr).Free();
      return ErlNifTerm.Zero;
    }

    // NOTE: A big-ass todo here, ProcessInit is currently passed to C via 'spawn'
    // and the C is responsible for turning the void* into a resource to go to erlang
    // and then mirroring that on the return
    // All the other process callbacks have the C# create the resource and unmap it
    // but the code is presently assymetrical so that needs sorting too
    public ErlNifTerm ProcessInit(ErlNifEnv env, IntPtr fn)
    {
      this.runtime.SetEnv(env);
      ProcessInit callback = Marshal.GetDelegateForFunctionPointer<ProcessInit>(fn);
      ITerm term = callback(new ProcessContext(this.runtime));
      return term.Native;
    }

    public ErlNifTerm ProcessMsg(ErlNifEnv env, ErlNifTerm fn, ErlNifTerm msg)
    {
      this.runtime.SetEnv(env);

      var fnResource = new Term(this.runtime, fn);
      IntPtr fnPtr = this.runtime.UnpackPointerResource(fnResource);
      this.runtime.ReleasePointerResource(fnResource);

      ProcessMsg callback = Marshal.GetDelegateForFunctionPointer<ProcessMsg>(fnPtr);
      ITerm term = callback(new ProcessContext(this.runtime), new Term(this.runtime, msg));
      return term.Native;
    }

    public ErlNifTerm ProcessTimeout(ErlNifEnv env, ErlNifTerm fn)
    {
      this.runtime.SetEnv(env);

      var fnResource = new Term(this.runtime, fn);
      IntPtr fnPtr = this.runtime.UnpackPointerResource(fnResource);
      this.runtime.ReleasePointerResource(fnResource);

      ProcessMsg callback = Marshal.GetDelegateForFunctionPointer<ProcessMsg>(fnPtr);
      ITerm term = callback(new ProcessContext(this.runtime), null);
      return term.Native;
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
          return this.runtime.MakeAtom("enoent").Native;
        }

        var ctor = appType.GetConstructor(Type.EmptyTypes);

        this.running_app = (IApp)ctor.Invoke(new object[]{});

        var term = this.running_app.Start(this.runtime);

        return term.Native;
      }
      // I think I just need to do this or we're going to end up with exceptions bubbling up into C
      // and we'll end up leaking shit all over the place
      catch (Exception ex) {
        Console.WriteLine("Exception in bridge: " + ex.ToString());
        return this.runtime.MakeAtom("error").Native;
      }
    }
  }
}
