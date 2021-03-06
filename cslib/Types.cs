using System;

namespace CsLib.Types 
{
  public sealed class ErlangPid 
  {

  }


  public sealed class ErlangRuntime 
  {
    public ErlangPid spawn() 
    {
      Console.WriteLine("This got called, fucking hell");
      return new ErlangPid();
    }
  }
}
