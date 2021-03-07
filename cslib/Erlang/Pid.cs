using System;
using CsLib;


namespace CsLib.Erlang 
{
  public sealed class Pid : ITerm
  {
    int handle;
    Runtime runtime;

    internal Pid(Runtime runtime, int handle) 
    {
      this.handle = handle;
      this.runtime = runtime;
    }
    
    public int Handle() { return this.handle; }
  }
}
