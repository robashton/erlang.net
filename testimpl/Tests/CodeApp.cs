using System;
using CsLib.Erlang;
using CsLib;

namespace TestImpl.Tests
{
  public class CodeAppGenServer : IHandleInfo<Object>
  {
    Pid fileHandle = Pid.Zero;

    public CodeAppGenServer() {
    }

    public HandleInfoResult HandleInfo(HandleInfoContext ctx, Object msg) {
      switch(msg) {
        case Tuple<Atom, Pid, String, Byte[]> c when c.Item1 == "write": 
          Atom result = (Atom)Erlang.Modules.File.WriteFile(c.Item3, c.Item4);
          Erlang.Send(c.Item2, new Atom(result));
          break;
        case Tuple<Atom, String> cmd when cmd.Item1 == "open":
          Tuple <Atom, Pid> success = (Tuple<Atom,Pid>)Erlang.Modules.File.Open(cmd.Item2, new object[] { new Atom("write") } );
          this.fileHandle = success.Item2;
          break;
        case Tuple<Atom, byte[]> cmd when cmd.Item1 == "write":
          Erlang.Modules.File.Write(this.fileHandle, cmd.Item2);
          break;
        case Tuple<Atom, Pid> cmd when cmd.Item1 == "close":
          Erlang.Modules.File.Close(this.fileHandle);
          Erlang.Send(cmd.Item2, new Atom("ok"));
          break;
      }
      return ctx.NoReply();
    }
  }

    public class CodeApp : IApp
    {
      public Object Start()
      {
        return GenServer.StartLink( () => new CodeAppGenServer());
      }
    }
}
