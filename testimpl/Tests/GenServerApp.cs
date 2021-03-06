using System;
using Erlang;

using InfoMsg = System.Tuple<System.String, Erlang.Pid>;
using CallMsg = System.Object;

namespace TestImpl.Tests
{

  public class MyGenServer : IHandleInfo<InfoMsg>
                           , IHandleCall<CallMsg>
                           , IHandleCast<InfoMsg>
                           , ITerminate
  {
    Pid owner = Pid.Zero;

    public MyGenServer() {}

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
        Erl.Send(this.owner, new Atom("bye"));
      }
      return ctx.Ok();
    }

    private void HandleInfoImpl(InfoMsg msg) {
      switch(msg) {
        case ( "hello bob", _ ): 
          Erl.Send(msg.Item2, "hello joe");
          break;
        case ( _, _ ): 
          Erl.Send(msg.Item2, "woah");
          break;
      }
    }
  }

  public class GenServerApp : IApp
  {
    public Object Start()
    {
      return GenServer.StartLink(() => new MyGenServer() );
    }
  }
}
