using System;
using CsLib;
using System.Linq;

using System.Runtime.InteropServices;


namespace CsLib.Erlang
{
  public sealed class GenServer {

    public static void Stop(Runtime runtime, Pid pid) {
      runtime.Modules.GenServer.Stop(pid);
    }

    public static Pid StartLink<T>(Runtime runtime, Atom name, Func<T> init)  {
      return StartLinkInternal(runtime, Tuple.Create(new Atom("local"), name), init);
    }
    
    public static Pid StartLink<T>(Runtime runtime, Func<T> init) 
    {
      return StartLinkInternal(runtime, null, init);
    }

    private static Pid StartLinkInternal<T>(Runtime runtime, Object name, Func<T> init) 
    {
      var handleInfoInterface = typeof(T).GetInterfaces()
                                         .Where(x => x.IsGenericType && x.GetGenericTypeDefinition() == typeof(IHandleInfo<>))
                                         .FirstOrDefault();

      var handleCallInterface = typeof(T).GetInterfaces()
                                         .Where(x => x.IsGenericType && x.GetGenericTypeDefinition() == typeof(IHandleCall<>))
                                         .FirstOrDefault();

      var handleCastInterface = typeof(T).GetInterfaces()
                                         .Where(x => x.IsGenericType && x.GetGenericTypeDefinition() == typeof(IHandleCast<>))
                                         .FirstOrDefault();

      var terminateInterface = typeof(T).GetInterfaces()
                                         .Where(x => x == typeof(ITerminate))
                                         .FirstOrDefault();

      ErlangCallback initCallback = (Runtime runtime, ErlNifTerm obj) => {
        var genserver = init();
        return runtime.MakeTuple2(runtime.MakeAtom("ok"), runtime.MakeObjectReference(genserver));
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

      ErlangCallback handleCastCallback = (Runtime runtime, ErlNifTerm input) => {
        var args = runtime.Coerce<Tuple<ErlNifTerm, ErlNifTerm>>(input);
        var msgType = handleCastInterface.GetGenericArguments()[0];
        var msg = runtime.Coerce(args.Item1, msgType);
        var state = runtime.GetObjectReference(args.Item2);
        HandleCastResult result = (HandleCastResult)handleCastInterface.GetMethod("HandleCast").Invoke(state, new object[] { new HandleCastContext(runtime, state),  msg });
        return result.Native;
      }; 

      ErlangCallback terminateCallback = (Runtime runtime, ErlNifTerm input) => {
        var args = runtime.Coerce<Tuple<ErlNifTerm, ErlNifTerm>>(input);
        var reason = runtime.Coerce<Atom>(args.Item1);
        var state = runtime.GetObjectReference(args.Item2);
        TerminateResult result = (TerminateResult)terminateInterface.GetMethod("Terminate").Invoke(state, new object[] { new TerminateContext(runtime), reason });
        return result.Native;
      }; 

      var callbacks = new DotNetGenServerArgs { Init = initCallback
                                              , HandleInfo = handleInfoInterface == null ? null : handleInfoCallback
                                              , HandleCall = handleCallInterface == null ? null : handleCallCallback
                                              , HandleCast = handleCastInterface == null ? null : handleCastCallback
                                              , Terminate = terminateInterface == null ? null : terminateCallback
      };
      
      Object result;

      if(name != null) {
        result = runtime.Modules.DotnetGenserver.StartLink(name, callbacks);
      } else {
        result = runtime.Modules.DotnetGenserver.StartLink(callbacks);
      }

      switch(result) {
        case Tuple<Atom, Pid> success: 
          return success.Item2;
        case Tuple<Atom, Atom> error: 
          throw new Exception("Got an error when starting gen server " + error.Item2);
        default:
          throw new Exception("Failed to start the gen server for unknown reasons");
      }
    }
  }

  public record DotNetGenServerArgs
  {
    public ErlangCallback Init { get; init; }
    public ErlangCallback HandleInfo { get; init; }
    public ErlangCallback HandleCall { get; init; }
    public ErlangCallback HandleCast { get; init; }
    public ErlangCallback Terminate { get; init; }
  }

  public interface IHandleCall<Msg> {
    HandleCallResult HandleCall(HandleCallContext context, Msg msg);
  }
  public interface IHandleCast<Msg> {
    HandleCastResult HandleCast(HandleCastContext context, Msg msg);
  }

  public interface ITerminate {
    TerminateResult Terminate(TerminateContext context, Atom reason);
  }
  public interface IHandleInfo<Msg> {
    HandleInfoResult HandleInfo(HandleInfoContext context, Msg msg);
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

  public sealed class HandleInfoResult : GenResult 
  {
    internal HandleInfoResult(ErlNifTerm term) : base(term) {}
  }

  public sealed class HandleCastResult : GenResult 
  {
    internal HandleCastResult(ErlNifTerm term) : base(term) {}
  }

  public sealed class TerminateResult : GenResult 
  {
    internal TerminateResult(ErlNifTerm term) : base(term) {}
  }

  public sealed class HandleInfoContext {
    Runtime runtime;
    Object genserver;

    internal HandleInfoContext(Runtime runtime, Object genserver) {
      this.runtime = runtime;
      this.genserver = genserver;
    }

    public HandleInfoResult NoReply() {
      return new (runtime.MakeTuple2(runtime.MakeAtom("noreply"), runtime.MakeObjectReference(genserver)));
    }
  }

  public sealed class HandleCastContext {
    Runtime runtime;
    Object genserver;

    internal HandleCastContext(Runtime runtime, Object genserver) {
      this.runtime = runtime;
      this.genserver = genserver;
    }

    public HandleCastResult NoReply() {
      return new (runtime.MakeTuple2(runtime.MakeAtom("noreply"), runtime.MakeObjectReference(genserver)));
    }
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
      return new (runtime.MakeTuple3(
              runtime.MakeAtom("reply"),
              result,
              runtime.MakeObjectReference(genserver)));
    }
  }

  public sealed class TerminateContext {
    Runtime runtime;

    internal TerminateContext(Runtime runtime) {
      this.runtime = runtime;
    }

    public TerminateResult Ok() {
      return new (runtime.MakeAtom("ok"));
    }
  }


}