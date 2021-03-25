using System;
using Erlang;

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
          Atom result = (Atom)Erl.Modules.File.WriteFile(c.Item3, c.Item4);
          Erl.Send(c.Item2, new Atom(result));
          break;
        case Tuple<Atom, String> cmd when cmd.Item1 == "open":
          Tuple <Atom, Pid> success = (Tuple<Atom,Pid>)Erl.Modules.File.Open(cmd.Item2, new object[] { new Atom("write") } );
          this.fileHandle = success.Item2;
          break;
        case Tuple<Atom, byte[]> cmd when cmd.Item1 == "write":
          Erl.Modules.File.Write(this.fileHandle, cmd.Item2);
          break;
        case Tuple<Atom, Pid> cmd when cmd.Item1 == "close":
          Erl.Modules.File.Close(this.fileHandle);
          Erl.Send(cmd.Item2, new Atom("ok"));
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
