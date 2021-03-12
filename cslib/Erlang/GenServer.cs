using System;
using CsLib;

using System.Runtime.InteropServices;


namespace CsLib.Erlang
{
  public delegate GenInitResult GenInit<T>(GenInitContext<T> ctx);

  public sealed class GenInitContext<T> {
    private Runtime runtime;

    internal GenInitContext(Runtime runtime) {
      this.runtime = runtime;
    }

    public GenInitResult Ok(T state) {
      return new GenInitResult(runtime.MakeTuple2(
            runtime.MakeAtom("ok"),
            runtime.MakeObjectReference(state))); 
    }
  }

  public class GenInitResult {
    private ErlNifTerm term;

    internal GenInitResult(ErlNifTerm term) {
      this.term = term;
    }
    
    public ErlNifTerm Native { get { return this.term; }}
  }

  public sealed class GenInitResult<T> : GenInitResult {
    internal GenInitResult(ErlNifTerm term) : base(term) { }
  }


  public sealed class GenServer {
    
    // Aha, so this means that I'll *need* to use the resource mgmt in Erlang
    // to clear up my gchandles cos Erlang will *need* to hold onto this for the duration
    // Hopefully we can do this without creating callbacks for every single other bloody callback
    public static ErlNifTerm StartLink<T>(Runtime runtime, GenInit<T> init) 
    {
      ErlangCallback del = (Runtime runtime, ErlNifTerm obj) => {
        var result = init(new GenInitContext<T>(runtime));
        return result.Native;
      };
      var resource = runtime.MakeObjectReference(del);
      return runtime.CallErlangFn("dotnetgenserver", "start_link", new [] { resource }); 
    }
  }


  public sealed class HandleInfoContext {
    
  }

  // Probably something like this?
  public interface IHandleInfo<Msg> {
    void HandleInfo(HandleInfoContext context, Msg msg);
  }
}
