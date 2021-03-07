using System;
using CsLib.Types;
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
    public IntPtr increment;
    public IntPtr load_assembly;
  }

  public class Bridge {

    int value = 0;
    
    unsafe public static int Create(IntPtr ptr, int argLength) {
      CreateArgs* args = (CreateArgs*)ptr;
      Bridge instance = new Bridge();
      var handle = GCHandle.Alloc(instance);
      args->handle = GCHandle.ToIntPtr(handle);


      args->increment = (IntPtr)(delegate*<IntPtr, int>) &IncrementWrapper;
      args->@return = (IntPtr)(delegate* <IntPtr, int>) &Return;
      args->load_assembly = (IntPtr)(delegate*<IntPtr, IntPtr, int>)&LoadAssemblyWrapper;
      return 0;
    }
    

    static int IncrementWrapper(IntPtr ptr) {
      return ((Bridge)(GCHandle.FromIntPtr(ptr).Target)).Increment();
    }

    static int LoadAssemblyWrapper(IntPtr ptr, IntPtr assemblyName) {
      return ((Bridge)(GCHandle.FromIntPtr(ptr).Target)).LoadAssembly(Marshal.PtrToStringAuto(assemblyName));
    }

    public static int Return(IntPtr hptr) {
      GCHandle.FromIntPtr(hptr).Free(); 
      return 0; 
    }

    public int Increment() {
      return this.value += 1;
    }

   public int LoadAssembly(String assemblyName) {
     var assembly = Assembly.LoadFrom(assemblyName);
     var appType = assembly.GetExportedTypes()
                     .FirstOrDefault(t => t.GetInterface(typeof(IApp).Name) != null);

     var ctor = appType.GetConstructor(Type.EmptyTypes);

     var instance = ctor.Invoke(new object[]{});

     Console.WriteLine("Fuckadoodledo: " + assemblyName);
     return 0;
   }
  }
}
