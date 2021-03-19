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
  }
  public class NamedGenServersApp : IApp
  {
    Runtime runtime;
    Pid workerOne = Pid.Zero;
    Pid workerTwo = Pid.Zero;

    public Object Start(Runtime runtime)
    {
      this.runtime = runtime;
      return Process.Spawn(runtime, WorkerInit);
    }

    ProcessResult WorkerInit(Process ctx) {
      workerOne = GenServer.StartLink(runtime, new Atom("named-one"), () => new MyNamedGenServer("one"));
      workerTwo = GenServer.StartLink(runtime, new Atom("named-two"), () => new MyNamedGenServer("two"));
      return ctx.Receive((Process ctx, Object msg) => WorkerLoopReceive(ctx, msg));
    }

    ProcessResult WorkerLoopReceive(Process ctx, Object msg)
    {
      GenServer.Stop(runtime, workerOne);
      GenServer.Stop(runtime, workerTwo);
      return ctx.Finish(this.runtime.MakeAtom("ok"));
    }
  }
}
