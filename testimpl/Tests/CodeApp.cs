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
          ErlNifTerm result = this.runtime.Modules.File.WriteFile(c.Item3, c.Item4);
          switch (this.runtime.ExtractAuto(result)) {
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
        var pid = GenServer.StartLink<CodeAppGenServer>(runtime, (ctx) => ctx.Ok(new CodeAppGenServer(runtime)));
        return runtime.MakeTuple2( runtime.MakeAtom("ok"), pid );
      }
    }
}
