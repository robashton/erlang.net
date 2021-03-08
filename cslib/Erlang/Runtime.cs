using System;
using System.Runtime.InteropServices;
using System.Threading;

namespace CsLib.Erlang 
{

  [StructLayout(LayoutKind.Sequential)]
  public struct RuntimeImpl
  {
    public IntPtr spawn;
    public IntPtr makeAtom;
    public IntPtr receive;
    public IntPtr receiveWithTimeout;
  }

  public delegate ITerm ProcessCallback(Runtime runtime);

  public unsafe sealed class Runtime 
  {
    private delegate* <IntPtr, IntPtr, int> spawn;
    private delegate* <IntPtr, IntPtr, int> makeAtom;
    private delegate* <IntPtr, int> receive;
    private delegate* <IntPtr, int, int> receiveWithTimeout;

    private ThreadLocal<IntPtr> env = new ThreadLocal<IntPtr>();

    internal void SetEnv(IntPtr env) {
      this.env.Value = env;
    }

    internal Runtime(IntPtr runtime) {
      RuntimeImpl* impl = (RuntimeImpl*)runtime;
      this.spawn = (delegate* <IntPtr, IntPtr, int>)impl->spawn;
      this.makeAtom = (delegate* <IntPtr, IntPtr, int>)impl->makeAtom;
      this.receive = (delegate* <IntPtr, int>)impl->receive;
      this.receiveWithTimeout = (delegate* <IntPtr, int, int>)impl->receiveWithTimeout;
    }

    public ITerm Receive(int? timeout) 
    {
      var term = timeout.HasValue 
                    ? this.receiveWithTimeout(this.env.Value, timeout.Value) 
                    : this.receive(this.env.Value);

      return new Term(this, term);
    }
    
    public Pid Spawn(ProcessCallback fn)  
    {
      IntPtr ptr = Marshal.GetFunctionPointerForDelegate(fn);
      var result = this.spawn(this.env.Value, ptr);
      return new Pid(this, result);
    }

    public Atom MakeAtom(String str) {
      IntPtr strPtr = (IntPtr)Marshal.StringToHGlobalAnsi(str);
      var result = this.makeAtom(this.env.Value, strPtr);
      Marshal.FreeHGlobal(strPtr);
      return new Atom(this, result);
    }
  }
}
