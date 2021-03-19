using System;
using CsLib;
using CsLib.Erlang;

using InfoMsg = System.Tuple<System.String, CsLib.Erlang.Pid>;
using CallMsg = System.Object;

namespace TestImpl.Tests
{

  public class MyGenServer : IHandleInfo<InfoMsg>
                           , IHandleCall<CallMsg>
                           , IHandleCast<InfoMsg>
                           , ITerminate
  {
    Runtime runtime;
    Pid owner = Pid.Zero;

    public MyGenServer(Runtime runtime) {
      this.runtime = runtime;
    }

    public HandleInfoResult HandleInfo(HandleInfoContext ctx, InfoMsg msg) {
      HandleInfoImpl(msg);
      return ctx.NoReply();
    }

    public HandleCastResult HandleCast(HandleCastContext ctx, InfoMsg msg) {
      HandleInfoImpl(msg);
      return ctx.NoReply();
    }

    public HandleCallResult HandleCall(HandleCallContext ctx, Object msg) {
      switch(msg) {
        case "hello bob": 
          return ctx.Reply("hello joe");
        case Tuple<String, Pid> t when t.Item1 == "store": 
          this.owner = t.Item2;
          return ctx.Reply(new Atom("ok"));
        default: 
          return ctx.Reply("nope");
      }
    }

    public TerminateResult Terminate(TerminateContext ctx, Atom reason) {
      if(this.owner.HasValue) {
        this.runtime.Send(this.owner, runtime.MakeAtom("bye"));
      }
      return ctx.Ok();
    }

    private void HandleInfoImpl(InfoMsg msg) {
      switch(msg) {
        case ( "hello bob", _ ): 
          runtime.Send(msg.Item2, runtime.ExportAuto("hello joe"));
          break;
        case ( _, _ ): 
          runtime.Send(msg.Item2, runtime.ExportAuto("boobs"));
          break;
      }
    }
  }

    public class GenServerApp : IApp
    {

      public Object Start(Runtime runtime)
      {
        return GenServer.StartLink(runtime, () => new MyGenServer(runtime) );
      }
    }
}
