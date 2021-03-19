using System;
using CsLib;
using CsLib.Erlang;

using InfoMsg = System.Tuple<System.String, CsLib.Erlang.Pid>;
using CallMsg = System.Object;

namespace TestImpl.Tests
{

  public class MyNamedGenServer : IHandleCall<CallMsg>
  {
    String name;

    public MyNamedGenServer(String name) {
      this.name = name;
    }

    public HandleCallResult HandleCall(HandleCallContext ctx, Object msg) {
      return ctx.Reply(this.name);
    }

    public class NamedGenServersApp : IApp
    {
      Runtime runtime;

      public Object Start(Runtime runtime)
      {
        this.runtime = runtime;
        var pid = Process.Spawn(runtime, WorkerInit);
        return new Tuple<Atom, Pid>(new Atom("ok"), pid);
      }

      ProcessResult WorkerInit(Process ctx) {
//        GenServer.StartLink<MyNamedGenServer>(runtime, new Atom("named-one"), () => ctx.Ok(new MyNamedGenServer("one")));
//        GenServer.StartLink<MyNamedGenServer>(runtime, new Atom("named-two"), () => ctx.Ok(new MyNamedGenServer("two")));
        return ctx.Receive((Process ctx, Object msg) => WorkerLoopReceive(ctx, msg));
      }

      ProcessResult WorkerLoopReceive(Process ctx, Object msg)
      {
        return ctx.Finish(this.runtime.MakeAtom("ok"));
      }
    }
  }
}
