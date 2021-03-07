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
    public IntPtr @return;
    public IntPtr load_assembly;
    public IntPtr runtime;
  }

  [StructLayout(LayoutKind.Sequential)]
  public struct AsyncRequest 
  {
    public int isComplete;
    public IntPtr result;
  }

  public class Bridge {

    int value = 0;
    
    unsafe public static int Create(IntPtr ptr, int argLength) {
      CreateArgs* args = (CreateArgs*)ptr;
      Bridge instance = new Bridge();
      var handle = GCHandle.Alloc(instance);
      args->handle = GCHandle.ToIntPtr(handle);


      args->@return = (IntPtr)(delegate* <IntPtr, int>) &Return;
      args->load_assembly = (IntPtr)(delegate*<IntPtr, IntPtr, IntPtr>)&LoadAssemblyWrapper;
      return 0;
    }
    
    static IntPtr LoadAssemblyWrapper(IntPtr ptr, IntPtr assemblyName) {
      return ((Bridge)(GCHandle.FromIntPtr(ptr).Target)).LoadAssembly(Marshal.PtrToStringAuto(assemblyName));
    }

    public static int Return(IntPtr hptr) {
      GCHandle.FromIntPtr(hptr).Free(); 
      return 0; 
    }

   public IntPtr LoadAssembly(String assemblyName) {

     try
     {
       var assembly = Assembly.LoadFrom(assemblyName);
       var appType = assembly.GetExportedTypes()
                       .FirstOrDefault(t => t.GetInterface(typeof(IApp).Name) != null);

       var ctor = appType.GetConstructor(Type.EmptyTypes);

       var instance = ctor.Invoke(new object[]{});

       var handle = GCHandle.Alloc(instance);
       return GCHandle.ToIntPtr(handle);
     }
     // I think I just need to do this or we're going to end up with exceptions bubbling up into C
     // and we'll end up leaking shit all over the place
     catch (Exception ex) {
       Console.WriteLine("Exception in bridge: " + ex.ToString());
       return IntPtr.Zero;
     }
   }
  }
}
