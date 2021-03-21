using System;
using CsLib.Erlang;
using CsLib;

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
  }
  public class NamedGenServersApp : IApp
  {
    Pid workerOne = Pid.Zero;
    Pid workerTwo = Pid.Zero;

    public Object Start()
    {
      return Process.Spawn(WorkerInit);
    }

    ProcessResult WorkerInit(Process ctx) {
      workerOne = GenServer.StartLink(new Atom("named-one"), () => new MyNamedGenServer("one"));
      workerTwo = GenServer.StartLink(new Atom("named-two"), () => new MyNamedGenServer("two"));
      return ctx.Receive((Process ctx, Object msg) => WorkerLoopReceive(ctx, msg));
    }

    ProcessResult WorkerLoopReceive(Process ctx, Object msg)
    {
      GenServer.Stop(workerOne);
      GenServer.Stop(workerTwo);
      return ctx.Finish(Erlang.MakeAtom("ok"));
    }
  }
}
