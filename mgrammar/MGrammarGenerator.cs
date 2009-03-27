using System;
using System.IO;
using System.Text;

namespace Mono.MCompiler
{
	enum ParserMode { None, Lexer, Tokenizer }

	public class MLexerGenerator
	{
		public static void Main (string [] args)
		{
			var g = new MLexerGenerator ();
			g.Run (args);
		}

		public void Run (string [] args)
		{
			if (args.Length > 1)
				w = new StreamWriter (args [1]);
			using (w) {
				w.Write (head);
				Process (args [0]);
				w.Write (mid);
				w.Write (decl.ToString ());
				w.Write (tail);
			}
		}

		string head = @"
// ===============================================================
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Irony.Compiler;

namespace MCompiler
{
	public partial class MGrammar : Grammar
	{
		public MGrammar ()
		{
";

		string mid = @"
		}

		public StringLiteral L (string s)
		{
			return new StringLiteral (s);
		}

		public NonTerminal Opt (BnfTerm elem)
		{
			return new NonTerminal (elem.Name + "".opt"") { Rule = Empty + elem };
		}

		public NonTerminal NoneOf (BnfTerm term)
		{
			// FIXME: can we handle them?
			throw new NotImplementedException ();
		}

		BnfExpression GetSignToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetDateDaysToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetDateMonthsToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetNormalCharacterToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetLetterToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetDecimalDigitToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetHexDigitToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetTimeHoursToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression StringContent (char sep, bool verbatim)
		{
			throw new NotImplementedException ();
		}

		// some exceptional rules for literals that starts with an uppercase letter.
		StringLiteral T { get { return new StringLiteral (""T""); } }
		StringLiteral Z { get { return new StringLiteral (""Z""); } }
		StringLiteral E { get { return new StringLiteral (""E""); } }

		// ---- Non-terminals -----
";

		string tail = @"
		// ---- End of non-terminals ----
	}
}
";


		TextReader reader;
		string current_rule;
		TextWriter w = Console.Out;
		StringWriter decl = new StringWriter ();
		ParserMode mode;

		static readonly char [] eols = {'\r', '\n'};

		public void Process (string file)
		{
			reader = new StreamReader (file);
			foreach (var ss in reader.ReadToEnd ().Split (eols, StringSplitOptions.RemoveEmptyEntries)) {
				string s = StripComment (ss);
				if (s == null || s.Length == 0)
					continue;
				switch (s) {
				case "#Begin lexer rules": mode = ParserMode.Lexer; continue;
				case "#Begin tokenizer rules": mode = ParserMode.Tokenizer; continue;
				}
				switch (mode) {
				case ParserMode.Lexer: ProcessLexerRule (s); break;
				case ParserMode.Tokenizer: ProcessTokenRule (s); break;
				}
			}
			if (current_rule != null)
				w.WriteLine (";");
			w.WriteLine ();
		}

		static readonly char [] sep = {' '};

		int elements = 0;

		void ProcessLexerRule (string s)
		{
			// w.WriteLine ("// " + s); // no lexer generation this time.
			ProcessRule (s);
		}

		void ProcessTokenRule (string s)
		{
			ProcessRule (s);
		}

		void ProcessRule (string s)
		{
			s = s.Trim ();
			if (s.Length == 0)
				return;

			if (s.Length > 1 && s [s.Length - 1] == ':' && Char.IsLetter (s [s.Length - 2])) {
				if (current_rule != null)
					w.WriteLine (";");
				w.WriteLine ();
				elements = 0; // reset

				// start a new nonterminal
				current_rule = s.Substring (0, s.Length - 1);
				decl.WriteLine ("\t\tNonTerminal {0} = new NonTerminal (\"{0}\");", current_rule);
				w.WriteLine ("\t\t\t{0}.Rule = ", current_rule);
				w.Write ("\t\t\t\t");
			} else {
				// add an element to current nonterminal
				if (elements++ > 0) {
					w.WriteLine ();
					w.Write ("\t\t\t\t | ");
				}

				if (s.StartsWith ("one of", StringComparison.Ordinal)) {
					string [] tokens = s.Substring (6).Split (sep, StringSplitOptions.RemoveEmptyEntries);
					for (int i = 0; i < tokens.Length; i++) {
						if (i > 0)
							w.Write (" | ");
						ProcessRuleElement (tokens [i]);
					}
				} else if (s.StartsWith ("none of", StringComparison.Ordinal)) {
					string [] tokens = s.Substring (7).Split (sep, StringSplitOptions.RemoveEmptyEntries);
					w.Write ("NoneOf (");
					for (int i = 0; i < tokens.Length; i++) {
						if (i > 0)
							w.Write (" | ");
						ProcessRuleElement (tokens [i]);
					}
					w.Write (")");
				} else {
					string [] tokens = s.Split (sep, StringSplitOptions.RemoveEmptyEntries);
					for (int i = 0; i < tokens.Length; i++) {
						if (i > 0)
							w.Write (" + ");
						ProcessRuleElement (tokens [i]);
					}
				}
			}
		}

		void ProcessRuleElement (string s)
		{
			bool opt = s.EndsWith (".opt", StringComparison.Ordinal);
			if (opt) {
				s = s.Substring (0, s.Length - 4);
				w.Write ("Opt (");
			}
			if (s.StartsWith ("U+", StringComparison.Ordinal)) {
				w.Write ("L (\"\\u");
				w.Write (s.Substring (2));
				w.Write ("\")");
			} else if (Char.IsUpper (s, 0))
				w.Write (s); // named item
			else {
				w.Write ("L (\"");
				w.Write (s.Replace ("\\", "\\\\").Replace ("\"", "\\\""));
				w.Write ("\")");
			}
			if (opt)
				w.Write (")");
		}

		string StripComment (string s)
		{
			int idx = s != null ? s.IndexOf ("//", StringComparison.Ordinal) : -1;
			return (idx < 0) ? s : s.Substring (0, idx);
		}
	}
}

