using System;
using CsLib;

namespace CsLib.Erlang
{
  public readonly struct ErlNifTerm
  {
    public static readonly ErlNifTerm Zero = new ErlNifTerm(IntPtr.Zero);

    internal readonly IntPtr inner;

    internal ErlNifTerm(IntPtr inner) {
      this.inner = inner;
    }

    public bool HasValue
    {
      get { return this.inner != IntPtr.Zero; }
    }
  }
}
