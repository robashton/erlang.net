using System;
using System.Text.RegularExpressions;
using System.Dynamic;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Text;
using System.Reflection;
using System.Linq;
using CsLib;

namespace CsLib.Erlang
{
  public sealed class Functions : DynamicObject
  {
    private String moduleName;

    public Functions(String moduleName) {
      this.moduleName = moduleName;
    }
    
    public override bool TryInvokeMember(InvokeMemberBinder binder, object[] args, out object result) {
      ErlNifTerm term = Erlang.CallErlangFn(DotNetToErlang(moduleName), DotNetToErlang(binder.Name), args.Select(x => Erlang.ExportAuto(x)).ToArray());
      result = Erlang.ExtractAuto(term);
      if(result != null) { return true; }
      return false;
    }

    private static string DotNetToErlang(String str) {
      Regex pattern = new Regex(@"[A-Z][a-z]+");
      var matches = pattern.Matches(str);
      return string.Join("_", matches).ToLower();
    }
  }

}
