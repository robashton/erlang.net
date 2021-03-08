using System;
using CsLib;

namespace CsLib.Erlang 
{
  public class Term : ITerm
  {
    int handle;
    Runtime runtime;

    public bool HasValue 
    {
      get { return handle > 0; }
    }

    internal Term(Runtime runtime, int handle) 
    {
      this.handle = handle;
      this.runtime = runtime;
    }
    
    public int Handle() { return this.handle; }
  }
}
