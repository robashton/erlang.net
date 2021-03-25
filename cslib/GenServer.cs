using System;
using System.Linq;

using System.Runtime.InteropServices;
using Erlang;


namespace Erlang
{
  public sealed class GenServer {

    public static void Stop(Pid pid) {
      Erl.Modules.GenServer.Stop(pid);
    }

    public static Pid StartLink<T>(String name, Func<T> init)  {
      return StartLinkInternal(Tuple.Create(new Atom("local"), new Atom(name)), init);
    }
    
    public static Pid StartLink<T>(Func<T> init) 
    {
      return StartLinkInternal(null, init);
    }

    private static Pid StartLinkInternal<T>(Object name, Func<T> init) 
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

      ErlangCallback initCallback = (ErlNifTerm obj) => {
        var genserver = init();
        return Erl.MakeTuple2(Erl.MakeAtom("ok"), Erl.MakeObjectReference(genserver));
      };

      // Yes nasty Erlang reflection, we could actually cache all of these but who has the time of day?
      // If you're worried about performance then that means you're actually using this code
      // in which case you have bigger concerns than performance
      ErlangCallback handleInfoCallback = (ErlNifTerm input) => {
        var args = Erl.Coerce<Tuple<ErlNifTerm, ErlNifTerm>>(input);
        var msgType = handleInfoInterface.GetGenericArguments()[0];
        var msg = Erl.Coerce(args.Item1, msgType);
        var state = Erl.GetObjectReference(args.Item2);
        HandleInfoResult result = (HandleInfoResult)handleInfoInterface.GetMethod("HandleInfo").Invoke(state, new object[] { new HandleInfoContext(state),  msg });
        return result.Native;
      }; 

      ErlangCallback handleCallCallback = (ErlNifTerm input) => {
        var args = Erl.Coerce<Tuple<ErlNifTerm, ErlNifTerm, ErlNifTerm>>(input);
        var msgType = handleCallInterface.GetGenericArguments()[0];
        var msg = Erl.Coerce(args.Item1, msgType);
        var sender = Erl.Coerce<Pid>(args.Item2);
        var state = Erl.GetObjectReference(args.Item3);
        HandleCallResult result = (HandleCallResult)handleCallInterface.GetMethod("HandleCall").Invoke(state, new object[] { new HandleCallContext(sender, state),  msg });
        return result.Native;
      }; 

      ErlangCallback handleCastCallback = (ErlNifTerm input) => {
        var args = Erl.Coerce<Tuple<ErlNifTerm, ErlNifTerm>>(input);
        var msgType = handleCastInterface.GetGenericArguments()[0];
        var msg = Erl.Coerce(args.Item1, msgType);
        var state = Erl.GetObjectReference(args.Item2);
        HandleCastResult result = (HandleCastResult)handleCastInterface.GetMethod("HandleCast").Invoke(state, new object[] { new HandleCastContext(state),  msg });
        return result.Native;
      }; 

      ErlangCallback terminateCallback = (ErlNifTerm input) => {
        var args = Erl.Coerce<Tuple<ErlNifTerm, ErlNifTerm>>(input);
        var reason = Erl.Coerce<Atom>(args.Item1);
        var state = Erl.GetObjectReference(args.Item2);
        TerminateResult result = (TerminateResult)terminateInterface.GetMethod("Terminate").Invoke(state, new object[] { new TerminateContext(), reason });
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
        result = Erl.Modules.DotnetGenserver.StartLink(name, callbacks);
      } else {
        result = Erl.Modules.DotnetGenserver.StartLink(callbacks);
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
    Object genserver;

    internal HandleInfoContext( Object genserver) {
      this.genserver = genserver;
    }

    public HandleInfoResult NoReply() {
      return new (Erl.MakeTuple2(Erl.MakeAtom("noreply"), Erl.MakeObjectReference(genserver)));
    }
  }

  public sealed class HandleCastContext {
    Object genserver;

    internal HandleCastContext(Object genserver) {
      this.genserver = genserver;
    }

    public HandleCastResult NoReply() {
      return new (Erl.MakeTuple2(Erl.MakeAtom("noreply"), Erl.MakeObjectReference(genserver)));
    }
  }

  public sealed class HandleCallContext {
    Object genserver;
    Pid sender;
    
    public Pid Sender { get { return sender; }}

    internal HandleCallContext(Pid sender, Object genserver) {
      this.genserver = genserver;
      this.sender = sender;
    }

    public HandleCallResult Reply(Object result) {
      return Reply(Erl.ExportAuto(result));
    }

    public HandleCallResult Reply(ErlNifTerm result) {
      return new (Erl.MakeTuple3(
              Erl.MakeAtom("reply"),
              result,
              Erl.MakeObjectReference(genserver)));
    }
  }

  public sealed class TerminateContext {
    internal TerminateContext() {
    }

    public TerminateResult Ok() {
      return new (Erl.MakeAtom("ok"));
    }
  }


}
