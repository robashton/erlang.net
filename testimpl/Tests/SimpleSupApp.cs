using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
  public class WorkerGenServer1 
  {
    public WorkerGenServer1() {
    }
  }

    public class SimpleSupApp : IApp
    {
      public Object Start() {
        return Supervisor.StartLink("primary-sup", () => new SupervisorConfig(SupervisionStrategy.OneForAll,
              new SupervisorChild [] { new SupervisorWorker("c1", () => GenServer.StartLink("genserver-one", () => new WorkerGenServer1()))
                     , new SupervisorWorker("c2", () => GenServer.StartLink("genserver-two", () => new WorkerGenServer1()))
                     , new SupervisorWorker("c3", () => GenServer.StartLink("genserver-three", () => new WorkerGenServer1()))
                     , new Supervisor("s1", () => 
                         Supervisor.StartLink("secondary-sup", () => new SupervisorConfig(SupervisionStrategy.OneForOne, 
                           new [] { new SupervisorWorker("sc1", () => GenServer.StartLink("nested-one", () => new WorkerGenServer1()))
                                 ,  new SupervisorWorker("sc2", () => GenServer.StartLink("nested-two", () => new WorkerGenServer1()))
                                 ,  new SupervisorWorker("sc3", () => GenServer.StartLink("nested-three", () => new WorkerGenServer1()))
                                 })))
                     }));
      }
    }
}
