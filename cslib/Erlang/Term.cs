using System;
using CsLib;

namespace CsLib.Erlang
{
  public class Term : ITerm
  {
    ErlNifTerm native;
    Runtime runtime;

    public bool HasValue
    {
      get { return native.HasValue; }
    }

    internal Term(Runtime runtime, ErlNifTerm native)
    {
      this.native = native;
      this.runtime = runtime;
    }

    public ErlNifTerm Native {
      get { return this.native; }
    }

    public T As<T>() 
    {
      return this.runtime.Coerce<T>(this.native);
    }

  }
}
