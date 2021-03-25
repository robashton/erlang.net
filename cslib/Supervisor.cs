using System;
using System.Linq;
using System.Collections.Generic;

using System.Runtime.InteropServices;


namespace Erlang
{
  public sealed class Supervisor : SupervisorChild {
    public Supervisor(String id, Func<Pid> init) : base(id, init) {}

    public static Pid StartLink(String id, Func<SupervisorConfig> init) {

      ErlangCallback initCallback = (ErlNifTerm args) => {
        var config = init();
        return Erl.MakeTuple2(
            Erl.MakeAtom("ok"),
            config.ToErlNifTerm()
            );
      };

      switch(Erl.Modules.DotnetSupervisor.StartLink(Tuple.Create(new Atom("local"), new Atom(id)), initCallback)) {
        case Tuple<Atom, Pid> success:
          return success.Item2;
        case Tuple<Atom, Atom> error:
          throw new Exception("Failed to start supervisor with error " + error);
        default:
          throw new Exception("Failed to start supervisor for unknown reasons");
      }
    }
  }

  public sealed class SupervisorConfig {
    SupervisionStrategy strategy;
    SupervisorChild[] children;

    int? intensity = null;
    int? period = null;

    private record SupFlags {
      public Atom Strategy { get; init; }
      public int? Intensity { get; init; }
      public int? Period { get; init; }
    }

    private record SupChild {
      public Atom Id { get; init;  }
      public Tuple<Atom, Atom, ErlNifTerm> Start { get; init; }
      public Atom Type { get; init; }
      public Atom Shutdown { get; init; }
    }

    internal ErlNifTerm ToErlNifTerm() {
      return Erl.MakeTuple2(
          Erl.ExportAuto(
            new SupFlags {
             Strategy = strategy switch {
                            SupervisionStrategy.OneForOne => new Atom("one_for_one"),
                            SupervisionStrategy.OneForAll => new Atom("one_for_all"),
                            _ => throw new Exception("fuck off")
                          },
             Intensity = intensity,
             Period = period
            }),
          Erl.MakeList(
            children.Select(c => 
              c switch {
              SupervisorWorker worker => Erl.ExportAuto(new SupChild {
                                                Id = new Atom(worker.Id),
                                                Start = Tuple.Create(new Atom("dotnet_shim"), new Atom("callback"), Erl.MakeList( Erl.MakeObjectReference(worker.Init) )),
                                                Type = new Atom("worker"),
                                                Shutdown = null
                                              }),
              Supervisor supervisor => Erl.ExportAuto(new SupChild {
                                                Id = new Atom(supervisor.Id),
                                                Start = Tuple.Create(new Atom("dotnet_shim"), new Atom("callback"), Erl.MakeList( Erl.MakeObjectReference(supervisor.Init) )),
                                                Type = new Atom("supervisor"),
                                                Shutdown = new Atom("infinity")
                                              }),

              _ => throw new Exception("fuck off")
              }).ToArray()
            ));
    }

    public SupervisorConfig(SupervisionStrategy strategy, SupervisorChild[] children) : this(strategy, null, null, children) {}

    public SupervisorConfig(SupervisionStrategy strategy, int? intensity, int? period, SupervisorChild[] children) {
      this.strategy = strategy;
      this.children = children;
      this.intensity = intensity;
      this.period = period;
    }
  }


  public enum SupervisionStrategy { OneForOne, OneForAll }

  public class SupervisorChild {

    String id;
    Func<Pid> init;

    public String Id { get { return id; }}
    public ErlangCallback Init { get { return new ErlangCallback((ErlNifTerm x) => { 
        return Erl.ExportAuto(Tuple.Create(new Atom("ok"), init())); }); }}

    internal SupervisorChild(String id, Func<Pid> init) {
      this.id = id;
      this.init = init;
    }
  }

  public sealed class SupervisorWorker : SupervisorChild {
    public SupervisorWorker(String id, Func<Pid> init) : base(id, init) {}
  }
}
