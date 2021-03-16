using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
  public class CodeAppGenServer : IHandleInfo<Object>
  {
    Runtime runtime;

    public CodeAppGenServer(Runtime runtime) {
      this.runtime = runtime;
    }

    public HandleInfoResult HandleInfo(HandleInfoContext ctx, Object msg) {
      switch(msg) {
        case Tuple<Atom, Pid, String, Byte[]> c when c.Item1 == "write": 
          switch(this.runtime.Modules.File.WriteFile(c.Item3, c.Item4)) {
            case Atom a when a == "ok":
              this.runtime.Send(c.Item2, this.runtime.MakeAtom("ok"));
              break;
            case Tuple<Atom, Atom> error:
              this.runtime.Send(c.Item2, this.runtime.ExportAuto(error));
              break;
          }
          break;
      }
      return ctx.NoReply();
    }
  }

    public class CodeApp : IApp
    {
      Runtime runtime;

      public ErlNifTerm Start(Runtime runtime)
      {
        this.runtime = runtime;
        switch(GenServer.StartLink<CodeAppGenServer>(runtime, (ctx) => ctx.Ok(new CodeAppGenServer(runtime)))) {
          case Tuple<Atom, Pid> result:
            return runtime.MakeTuple2( runtime.MakeAtom(result.Item1), runtime.MakePid(result.Item2) );
          default:
            throw new InvalidOperationException();
        }
      }
    }
}
