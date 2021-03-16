using System;
using CsLib;
using System.Linq;

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
    public static Object StartLink<T>(Runtime runtime, GenInit<T> init) 
    {
      var handleInfoInterface = typeof(T).GetInterfaces()
                                         .Where(x => x.IsGenericType && x.GetGenericTypeDefinition() == typeof(IHandleInfo<>))
                                         .FirstOrDefault();

      var handleCallInterface = typeof(T).GetInterfaces()
                                         .Where(x => x.IsGenericType && x.GetGenericTypeDefinition() == typeof(IHandleCall<>))
                                         .FirstOrDefault();

      ErlangCallback initCallback = (Runtime runtime, ErlNifTerm obj) => {
        var result = init(new GenInitContext<T>(runtime));
        return result.Native;
      };

      // Yes nasty runtime reflection, we could actually cache all of these but who has the time of day?
      // If you're worried about performance then that means you're actually using this code
      // in which case you have bigger concerns than performance
      ErlangCallback handleInfoCallback = (Runtime runtime, ErlNifTerm input) => {
        var args = runtime.Coerce<Tuple<ErlNifTerm, ErlNifTerm>>(input);
        var msgType = handleInfoInterface.GetGenericArguments()[0];
        var msg = runtime.Coerce(args.Item1, msgType);
        var state = runtime.GetObjectReference(args.Item2);
        HandleInfoResult result = (HandleInfoResult)handleInfoInterface.GetMethod("HandleInfo").Invoke(state, new object[] { new HandleInfoContext(runtime, state),  msg });

        return result.Native;
      }; 

      ErlangCallback handleCallCallback = (Runtime runtime, ErlNifTerm input) => {
        var args = runtime.Coerce<Tuple<ErlNifTerm, ErlNifTerm, ErlNifTerm>>(input);
        var msgType = handleCallInterface.GetGenericArguments()[0];
        var msg = runtime.Coerce(args.Item1, msgType);
        var sender = runtime.Coerce<Pid>(args.Item2);
        var state = runtime.GetObjectReference(args.Item3);
        HandleCallResult result = (HandleCallResult)handleCallInterface.GetMethod("HandleCall").Invoke(state, new object[] { new HandleCallContext(runtime, sender, state),  msg });

        return result.Native;
      }; 

      var callbacks = new DotNetGenServerArgs { Init = initCallback
                                              , HandleInfo = handleInfoInterface == null ? null : handleInfoCallback
                                              , HandleCall = handleCallInterface == null ? null : handleCallCallback
      };

      return runtime.Modules.DotnetGenserver.StartLink(callbacks); 
    }
  }

  public record DotNetGenServerArgs
  {
    public ErlangCallback Init { get; init; }
    public ErlangCallback HandleInfo { get; init; }
    public ErlangCallback HandleCall { get; init; }
  }

  public class GenResult {
    ErlNifTerm term;
    internal GenResult(ErlNifTerm term) {
      this.term = term;
    }
    public ErlNifTerm Native { get { return this.term; }}
  }

  public sealed class HandleCallResult : GenResult 
  {
    internal HandleCallResult(ErlNifTerm term) : base(term) {}
  }

  public sealed class HandleCallContext {
    Runtime runtime;
    Object genserver;
    Pid sender;
    
    public Pid Sender { get { return sender; }}

    internal HandleCallContext(Runtime runtime, Pid sender, Object genserver) {
      this.runtime = runtime;
      this.genserver = genserver;
      this.sender = sender;
    }

    public HandleCallResult Reply(Object result) {
      return Reply(this.runtime.ExportAuto(result));
    }

    public HandleCallResult Reply(ErlNifTerm result) {
      return new HandleCallResult(runtime.MakeTuple3(
              runtime.MakeAtom("reply"),
              result,
              runtime.MakeObjectReference(genserver)));
    }
  }

  public interface IHandleCall<Msg> {
    HandleCallResult HandleCall(HandleCallContext context, Msg msg);
  }

  public sealed class HandleInfoResult : GenResult 
  {
    internal HandleInfoResult(ErlNifTerm term) : base(term) {}
  }

  public sealed class HandleInfoContext {
    Runtime runtime;
    Object genserver;

    internal HandleInfoContext(Runtime runtime, Object genserver) {
      this.runtime = runtime;
      this.genserver = genserver;
    }

    public HandleInfoResult NoReply() {
      return new HandleInfoResult(runtime.MakeTuple2(runtime.MakeAtom("noreply"), runtime.MakeObjectReference(genserver)));
    }
  }

  public interface IHandleInfo<Msg> {
    HandleInfoResult HandleInfo(HandleInfoContext context, Msg msg);
  }

}
