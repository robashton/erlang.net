using System;
using CsLib;

namespace CsLib.Erlang 
{
  public class Term : ITerm
  {
    Int64 handle;
    Runtime runtime;

    public bool HasValue 
    {
      get { return handle > 0; }
    }

    internal Term(Runtime runtime, Int64 handle) 
    {
      this.handle = handle;
      this.runtime = runtime;
    }
    
    public Int64 Handle() { return this.handle; }
  }
}
