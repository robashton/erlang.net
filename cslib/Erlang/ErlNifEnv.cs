using System;
using CsLib;
using System.Runtime.InteropServices;

namespace CsLib.Erlang
{
  [StructLayout(LayoutKind.Sequential)]
  public readonly struct ErlNifEnv
  {
    public static readonly ErlNifEnv Zero = new ErlNifEnv(IntPtr.Zero);

    internal readonly IntPtr inner;

    internal ErlNifEnv(IntPtr inner) {
      this.inner = inner;
    }

    public bool HasValue
    {
      get { return this.inner != IntPtr.Zero; }
    }
  }
}
