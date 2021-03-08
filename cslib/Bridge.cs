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
    public IntPtr run_process_loop;
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
      args->@return = (IntPtr)(delegate* <IntPtr, int>) &Return;
      args->load_assembly = (IntPtr)(delegate*<IntPtr, IntPtr, IntPtr, int>)&LoadAssemblyWrapper;
      args->run_process_loop = (IntPtr)(delegate*<IntPtr, IntPtr, IntPtr, int>)&RunProcessLoopWrapper;

      return 0;
    }
    
    static int LoadAssemblyWrapper(IntPtr env, IntPtr bridge, IntPtr assemblyName) {
      return ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).LoadAssembly(env, Marshal.PtrToStringAuto(assemblyName));
    }

    static int RunProcessLoopWrapper(IntPtr env, IntPtr bridge, IntPtr fn) {
      return ((Bridge)(GCHandle.FromIntPtr(bridge).Target)).RunProcessLoop(env, fn);
    }

    public static int Return(IntPtr hptr) {
      GCHandle.FromIntPtr(hptr).Free(); 
      return 0; 
    }

    public int RunProcessLoop(IntPtr env, IntPtr fn) 
    {
      this.runtime.SetEnv(env);
      ProcessCallback callback = (ProcessCallback)Marshal.GetDelegateForFunctionPointer(fn, typeof(ProcessCallback));
      ITerm term = callback(this.runtime);
      return term.Handle();
    }

    public int LoadAssembly(IntPtr env, String filepath) 
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

        return term.Handle();
      }
      // I think I just need to do this or we're going to end up with exceptions bubbling up into C
      // and we'll end up leaking shit all over the place
      catch (Exception ex) {
        Console.WriteLine("Exception in bridge: " + ex.ToString());
        return 0;
      }
    }
  }
}
