using System;
using CsLib;

using System.Runtime.InteropServices;


namespace CsLib.Erlang
{
  public delegate GenInitResult GenInit<T>(GenInitContext ctx);
  public delegate GenInitResult GenInit(GenInitContext ctx);

  public class GenInitContext {
    private Runtime runtime;

    internal GenInitContext(Runtime runtime) {
      this.runtime = runtime;
    }

    public GenInitResult Ok(Object state) {
      return new GenInitResult(runtime.MakeTuple2(
            runtime.MakeAtom("ok"),
            // interestingly, we'll need to tell erlang to invoke a destructor
            // on something holding a GC handle
            // or I think this will disappear
            runtime.MakeObjectReference(state))); 
    }
  }

  public sealed class GenInitContext<T> : GenInitContext {
    internal GenInitContext(Runtime runtime) : base(runtime) { 
    }

    public GenInitResult<T> Ok(T state) {
      return new GenInitResult<T>(base.Ok((Object)state).Native);
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
      return runtime.StartGenServer((ctx) => (GenInitResult)init(ctx));
    }
  }





  public sealed class HandleInfoContext {
    
  }

  // Probably something like this?
  public interface IHandleInfo<Msg> {
    void HandleInfo(HandleInfoContext context, Msg msg);
  }
}
