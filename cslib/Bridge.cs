using System;
using CsLib.Types;
using System.Runtime.InteropServices;

namespace CsLib
{
  delegate int Increment();
  delegate int Return(IntPtr hptr);
  
  [StructLayout(LayoutKind.Sequential)]
  public struct CreateArgs
  {
    public IntPtr handle;
    public IntPtr increment;
    public IntPtr @return;
  }

  public class Bridge {

    int value = 0;
    
    unsafe public static int Create(IntPtr ptr, int argLength) {
      CreateArgs* args = (CreateArgs*)ptr;
      Bridge instance = new Bridge();
      var handle = GCHandle.Alloc(instance);
      args->handle = GCHandle.ToIntPtr(handle);
      args->increment = Marshal.GetFunctionPointerForDelegate<Increment>(delegate() { return instance.Increment(); });
      args->@return = Marshal.GetFunctionPointerForDelegate<Return>(delegate(IntPtr hptr) { GCHandle.FromIntPtr(hptr).Free(); return 0; });
      return 0;
    }

    public int Increment() {
      return this.value += 1;
    }
    
  }
}
