using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
    public class SingleProcessApp : IApp
    {
      Runtime runtime;

      public ITerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        var pid = runtime.Spawn(WorkerLoop);
        return runtime.MakeTuple2( runtime.MakeAtom("ok"), pid );
      }

      ProcessResult WorkerLoop(ProcessContext ctx) {
        return ctx.Receive(WorkerLoopReceive);
      }

      ProcessResult WorkerLoopReceive(ProcessContext ctx, Term term)
      {
        if(term.As<String>() == "bye") {
          return ctx.Finish(this.runtime.MakeAtom("ok"));
        }

        switch(term.As<Tuple<String, Pid, Term>>()) {
          case var (cmd, pid, reply) when cmd == "send_me":
            this.runtime.Send(pid, reply);
            break;
        }
        return ctx.Receive(WorkerLoopReceive);
      }
    }
}
