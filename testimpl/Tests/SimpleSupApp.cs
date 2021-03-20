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
              new [] { new SupervisorWorker("c1", () => GenServer.StartLink(runtime, "genserver-one", () => new WorkerGenServer1(runtime)))
                     , new SupervisorWorker("c2", () => GenServer.StartLink(runtime, "genserver-two", () => new WorkerGenServer1(runtime)))
                     , new SupervisorWorker("c3", () => GenServer.StartLink(runtime, "genserver-three", () => new WorkerGenServer1(runtime)))
                     }));
      }
    }
}
