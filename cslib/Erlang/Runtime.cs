using System;

namespace CsLib.Erlang 
{
  public sealed class Runtime 
  {
    public Pid spawn() 
    {
      Console.WriteLine("This got called, fucking hell");
      return new Pid();
    }
  }
}
