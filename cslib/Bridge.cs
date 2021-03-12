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
    public delegate* <ErlNifEnv, IntPtr, ErlNifTerm, ErlNifTerm> process_init;
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

      Runtime runtime = new Runtime();
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

    static ErlNifTerm ProcessInitWrapper(ErlNifEnv env, IntPtr bridge, ErlNifTerm fn) {
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

    // These should all perhaps be callbacks
    // sat on ProcessContext
    // and the pointer resource dance should
    // be done only once by a dedicated 'this is a callback function' handler
    // pretty sure that can just be 'callback, this is my fn, these are my args, thanks'
    public ErlNifTerm ProcessInit(ErlNifEnv env, ErlNifTerm fn)
    {
      this.runtime.SetEnv(env);
      ProcessInit callback = (ProcessInit)this.runtime.PointerResourceToDelegate(fn);
      ProcessResult result = callback(new ProcessContext(this.runtime));
      return result.Native;
    }

    public ErlNifTerm ProcessMsg(ErlNifEnv env, ErlNifTerm fn, ErlNifTerm msg)
    {
      this.runtime.SetEnv(env);
      ProcessMsg callback = (ProcessMsg)this.runtime.PointerResourceToDelegate(fn);
      ProcessResult result = callback(new ProcessContext(this.runtime), msg);
      return result.Native;
    }

    public ErlNifTerm ProcessTimeout(ErlNifEnv env, ErlNifTerm fn)
    {
      this.runtime.SetEnv(env);
      ProcessMsg callback = (ProcessMsg)this.runtime.PointerResourceToDelegate(fn);
      ProcessResult result = callback(new ProcessContext(this.runtime), ErlNifTerm.Zero);
      return result.Native;
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
