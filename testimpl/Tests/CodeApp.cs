using System;
using CsLib;
using CsLib.Erlang;

namespace TestImpl.Tests
{
  public class CodeAppGenServer : IHandleInfo<Object>
  {
    Runtime runtime;
    Pid fileHandle = Pid.Zero;

    public CodeAppGenServer(Runtime runtime) {
      this.runtime = runtime;
    }

    public HandleInfoResult HandleInfo(HandleInfoContext ctx, Object msg) {
      switch(msg) {
        case Tuple<Atom, Pid, String, Byte[]> c when c.Item1 == "write": 
          Atom result = (Atom)this.runtime.Modules.File.WriteFile(c.Item3, c.Item4);
          this.runtime.Send(c.Item2, this.runtime.MakeAtom(result));
          break;
        case Tuple<Atom, String> cmd when cmd.Item1 == "open":
          Tuple <Atom, Pid> success = (Tuple<Atom,Pid>)this.runtime.Modules.File.Open(cmd.Item2, new object[] { new Atom("write") } );
          this.fileHandle = success.Item2;
          break;
        case Tuple<Atom, byte[]> cmd when cmd.Item1 == "write":
          this.runtime.Modules.File.Write(this.fileHandle, cmd.Item2);
          break;
        case Tuple<Atom, Pid> cmd when cmd.Item1 == "close":
          this.runtime.Modules.File.Close(this.fileHandle);
          this.runtime.Send(cmd.Item2, this.runtime.MakeAtom("ok"));
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
