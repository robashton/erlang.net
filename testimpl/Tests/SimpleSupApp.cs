using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
//  public class WorkerGenServer1 : IHandleCall<Object>
//  {
//    Runtime runtime;
//    Pid fileHandle = Pid.Zero;
//
//    public WorkerGenServer1(Runtime runtime) {
//      this.runtime = runtime;
//    }
//  }

//    public class SimpleSupApp : IApp
//    {
//      Runtime runtime;
//
//      public Object Start(Runtime runtime)
//      {
//        this.runtime = runtime;
//
//        return Supervisor.StartLink((ctx) => ctx.Ok( SupervisionStrategy.OneForOne, 10, 10, 
//              new [] { new SupervisorWorker("one", () => GenServer.StartLink("one", () => new WorkerGenServer1(runtime)))
//                     , new SupervisorWorker("two", () => GenServer.StartLink("two", () => new WorkerGenServer1(runtime)))
//                     }));
//      }
//    }
}
