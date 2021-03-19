using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
  public class WorkerGenServer1 
  {
    Runtime runtime;

    public WorkerGenServer1(Runtime runtime) {
      this.runtime = runtime;
    }
  }

    public class SimpleSupApp : IApp
    {
      Runtime runtime;

      public Object Start(Runtime runtime)
      {
        this.runtime = runtime;

        return Supervisor.StartLink(runtime, "primary-sup", () => new SupervisorConfig(SupervisionStrategy.OneForAll,
              new [] { new SupervisorWorker("one", () => GenServer.StartLink(runtime, "one", () => new WorkerGenServer1(runtime)))
                     , new SupervisorWorker("two", () => GenServer.StartLink(runtime, "two", () => new WorkerGenServer1(runtime)))
                     , new SupervisorWorker("three", () => GenServer.StartLink(runtime, "three", () => new WorkerGenServer1(runtime)))
                     }));
      }
    }
}
