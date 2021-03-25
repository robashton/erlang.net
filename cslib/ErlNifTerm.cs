using System;
using System.Runtime.InteropServices;

namespace Erlang
{
  [StructLayout(LayoutKind.Sequential)]
  public readonly struct ErlNifTerm
  {
    public static readonly ErlNifTerm Zero = new (IntPtr.Zero);

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
