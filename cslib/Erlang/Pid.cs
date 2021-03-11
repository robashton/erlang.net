using System;
using CsLib;

namespace CsLib.Erlang
{
  public readonly struct Pid
  {
    public static readonly Pid Zero = new Pid(IntPtr.Zero);

    internal readonly IntPtr inner;

    internal Pid(IntPtr inner) {
      this.inner = inner;
    }

    public bool HasValue
    {
      get { return this.inner != IntPtr.Zero; }
    }
  }
}
