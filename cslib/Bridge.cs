using System;
using CsLib.Erlang;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Linq;



namespace CsLib
{
  [StructLayout(LayoutKind.Sequential)]
  public struct CreateArgs
  {
    public IntPtr handle;
    public IntPtr runtime;
    public IntPtr @return;
    public IntPtr load_assembly;
    public IntPtr process_init;
    public IntPtr process_msg;
    public IntPtr process_timeout;
  }

  public class Bridge {

    Runtime runtime;
    IApp running_app;

    private Bridge(Runtime runtime) {
      this.runtime = runtime;
    }

    unsafe public static int Create(IntPtr ptr, int argLength) {
      CreateArgs* args = (CreateArgs*)ptr;

      Runtime runtime = new Runtime(args->runtime);
      Bridge instance = new Bridge(runtime);

      var handle = GCHandle.Alloc(instance);
      args->handle = GCHandle.ToIntPtr(handle);
      args->@return = (IntPtr)(delegate* <IntPtr, ErlNifTerm>) &Return;
      args->load_assembly = (IntPtr)(delegate*<IntPtr, IntPtr, IntPtr, ErlNifTerm>)&LoadAssemblyWrapper;
      args->process_init = (IntPtr)(delegate*<IntPtr, IntPtr, IntPtr, ErlNifTerm>)&ProcessInitWrapper;
      args->process_msg = (IntPtr)(delegate*<IntPtr, IntPtr, ErlNifTerm, ErlNifTerm, ErlNifTerm>)&ProcessMsgWrapper;
      args->process_timeout = (IntPtr)(delegate*<IntPtr, IntPtr, ErlNifTerm, ErlNifTerm>)&ProcessTimeoutWrapper;

      return 0;
    }

    static ErlNifTerm LoadAssemblyWrapper(IntPtr env, IntPtr bridge, IntPtr assemblyName) {
      Console.WriteLine("In LoadAssemblyWrapper");
      var res = ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).LoadAssembly(env, Marshal.PtrToStringAuto(assemblyName));
      Console.WriteLine("Done With LoadAssemblyWrapper");
      return res;
    }

    static ErlNifTerm ProcessInitWrapper(IntPtr env, IntPtr bridge, IntPtr fn) {
      Console.WriteLine("In ProcessInitWrapper");
      var res = ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).ProcessInit(env, fn);
      Console.WriteLine("Done With ProcessInitWrapper");
      return res;
    }

    static ErlNifTerm ProcessMsgWrapper(IntPtr env, IntPtr bridge, ErlNifTerm fn, ErlNifTerm msg) {
      return ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).ProcessMsg(env, fn, msg);
    }

    static ErlNifTerm ProcessTimeoutWrapper(IntPtr env, IntPtr bridge, ErlNifTerm fn) {
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
    public ErlNifTerm ProcessInit(IntPtr env, IntPtr fn)
    {
      this.runtime.SetEnv(env);
      ProcessInit callback = Marshal.GetDelegateForFunctionPointer<ProcessInit>(fn);
      Console.WriteLine("Creating new process context");
      ITerm term = callback(new ProcessContext(this.runtime));
      Console.WriteLine("Created new process context");

      return term.Native;
    }

    public ErlNifTerm ProcessMsg(IntPtr env, ErlNifTerm fn, ErlNifTerm msg)
    {
      this.runtime.SetEnv(env);

      var fnResource = new Term(this.runtime, fn);
      IntPtr fnPtr = this.runtime.UnpackPointerResource(fnResource);
      this.runtime.ReleasePointerResource(fnResource);

      ProcessMsg callback = Marshal.GetDelegateForFunctionPointer<ProcessMsg>(fnPtr);
      ITerm term = callback(new ProcessContext(this.runtime), new Term(this.runtime, msg));
      return term.Native;
    }

    public ErlNifTerm ProcessTimeout(IntPtr env, ErlNifTerm fn)
    {
      this.runtime.SetEnv(env);

      var fnResource = new Term(this.runtime, fn);
      IntPtr fnPtr = this.runtime.UnpackPointerResource(fnResource);
      this.runtime.ReleasePointerResource(fnResource);

      ProcessMsg callback = Marshal.GetDelegateForFunctionPointer<ProcessMsg>(fnPtr);
      ITerm term = callback(new ProcessContext(this.runtime), null);
      return term.Native;
    }

    public ErlNifTerm LoadAssembly(IntPtr env, String filepath)
    {
      this.runtime.SetEnv(env);

      try
      {
        // TODO: Ask stears what he remembers about app domains and this shit..
        AssemblyName assemblyName = AssemblyName.GetAssemblyName(filepath);
        Assembly assembly = Assembly.Load(assemblyName);


        var appType = assembly.GetExportedTypes()
          .FirstOrDefault(t => t.GetInterface(typeof(IApp).Name) != null);

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
