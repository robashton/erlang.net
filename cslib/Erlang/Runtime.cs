using System;
using System.Runtime.InteropServices;

namespace CsLib.Erlang 
{

  [StructLayout(LayoutKind.Sequential)]
  public struct RuntimeImpl
  {
    public IntPtr env;
    public IntPtr spawn;
  }

  public unsafe sealed class Runtime 
  {
    private IntPtr env;
    private delegate* <IntPtr, int> spawn_;

    internal Runtime(IntPtr runtime) {
      RuntimeImpl* impl = (RuntimeImpl*)runtime;
      this.spawn_ = (delegate* <IntPtr, int>)impl->spawn;
      this.env = impl->env;
    }

    public Pid Spawn() 
    {
      Console.WriteLine("calling spawn on the runtime");
      var result = this.spawn_(this.env);
      Console.WriteLine("And returning a wrapper around the erlang term");
      return new Pid(this, result);
    }
  }
}
