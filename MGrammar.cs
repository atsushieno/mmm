
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

			NormalCharacter.Rule = 
				GetNormalCharacterToken();

			Input.Rule = 
				Opt (InputSection);

			InputSection.Rule = 
				InputSectionPart
				 | InputSection + InputSectionPart;

			InputSectionPart.Rule = 
				Opt (InputElements) + NewLine;

			InputElements.Rule = 
				InputElement
				 | InputElements + InputElement;

			InputElement.Rule = 
				Whitespace
				 | Token;

			NewLine.Rule = 
				NewLineCharacter
				 | L ("\u000D") + L ("\u000A");

			NewLineCharacter.Rule = 
				L ("\u000A")
				 | L ("\u000D")
				 | L ("\u0085")
				 | L ("\u2028")
				 | L ("\u2029");

			Whitespace.Rule = 
				WhitespaceCharacters;

			WhitespaceCharacter.Rule = 
				L ("\u0009")
				 | L ("\u000B")
				 | L ("\u000C")
				 | L ("\u0020")
				 | NewLineCharacter;

			WhitespaceCharacters.Rule = 
				WhitespaceCharacter
				 | WhitespaceCharacters + WhitespaceCharacter;

			Token.Rule = 
				Identifier
				 | Keyword
				 | Literal
				 | OperatorOrPunctuator;

			Identifier.Rule = 
				IdentifierBegin + Opt (IdentifierCharacters)
				 | IdentifierVerbatim;

			IdentifierBegin.Rule = 
				L ("_")
				 | Letter;

			IdentifierCharacter.Rule = 
				IdentifierBegin
				 | L ("$")
				 | DecimalDigit;

			IdentifierCharacters.Rule = 
				IdentifierCharacter
				 | IdentifierCharacters + IdentifierCharacter;

			IdentifierVerbatim.Rule = 
				L ("@[") + IdentifierVerbatimCharacters + L ("]");

			IdentifierVerbatimCharacter.Rule = 
				NoneOf (L ("]"))
				 | IdentifierVerbatimEscape;

			IdentifierVerbatimCharacters.Rule = 
				IdentifierVerbatimCharacter
				 | IdentifierVerbatimCharacters + IdentifierVerbatimCharacter;

			IdentifierVerbatimEscape.Rule = 
				L ("\\\\")
				 | L ("\\]");

			Letter.Rule = 
				GetLetterToken();

			DecimalDigit.Rule = 
				GetDecimalDigitToken() | L ("..") | L ("0..9");

			DecimalDigits.Rule = 
				DecimalDigit
				 | DecimalDigits + DecimalDigit;

			Keyword.Rule = 
				L ("any")
				 | L ("accumulate")
				 | L ("by")
				 | L ("empty")
				 | L ("equals")
				 | L ("error")
				 | L ("export")
				 | L ("false")
				 | L ("final")
				 | L ("from")
				 | L ("group")
				 | L ("id")
				 | L ("identity")
				 | L ("import")
				 | L ("in")
				 | L ("interleave")
				 | L ("join")
				 | L ("language")
				 | L ("labelof")
				 | L ("left")
				 | L ("let")
				 | L ("module")
				 | L ("null")
				 | L ("precedence")
				 | L ("right")
				 | L ("select")
				 | L ("syntax")
				 | L ("token")
				 | L ("true")
				 | L ("type")
				 | L ("unique")
				 | L ("value")
				 | L ("valuesof")
				 | L ("where");

			Literal.Rule = 
				DecimalLiteral
				 | IntegerLiteral
				 | ScientificLiteral
				 | DateTimeLiteral
				 | DateTimeOffsetLiteral
				 | TimeLiteral
				 | TextLiteral
				 | BinaryLiteral
				 | GuidLiteral
				 | LogicalLiteral
				 | NullLiteral;

			DecimalLiteral.Rule = 
				IntegerLiteral + L (".") + DecimalDigit + DecimalDigits;

			IntegerLiteral.Rule = 
				DecimalDigits;

			ScientificLiteral.Rule = 
				DecimalLiteral + L ("e") + Opt (Sign) + DecimalDigit + Opt (DecimalDigits)
				 | DecimalLiteral + E + Opt (Sign) + DecimalDigit + Opt (DecimalDigits);

			Sign.Rule = 
				L ("+") | L ("-");

			DateLiteral.Rule = 
				Opt (Sign) + DateYear + L ("-") + DateMonth + L ("-") + DateDay;

			DateDay.Rule = 
				L ("01") | L ("02") | L ("03") | L ("04") | L ("05") | L ("06") | L ("07") | L ("08") | L ("09") | L ("10") | L ("11") | L ("12") | L ("13") | L ("14") | L ("15") | L ("16") | L ("17") | L ("18") | L ("19") | L ("20") | L ("21") | L ("22") | L ("23") | L ("24") | L ("25") | L ("26") | L ("27") | L ("28") | L ("29") | L ("30") | L ("31");

			DateMonth.Rule = 
				L ("01") | L ("02") | L ("03") | L ("04") | L ("05") | L ("06") | L ("07") | L ("08") | L ("09") | L ("10") | L ("11") | L ("12");

			DateYear.Rule = 
				DecimalDigit + DecimalDigit + DecimalDigit + DecimalDigit;

			DateTimeLiteral.Rule = 
				DateLiteral + T + TimeLiteral;

			DateTimeOffsetLiteral.Rule = 
				DateLiteral + T + TimeLiteral + TimeZoneLiteral;

			TimeZoneLiteral.Rule = 
				Sign + TimeHourMinute
				 | Z;

			TimeLiteral.Rule = 
				TimeHourMinute + L (":") + TimeSecond;

			TimeHourMinute.Rule = 
				TimeHour + L (":") + TimeMinute;

			TimeHour.Rule = 
				L ("00") | L ("01") | L ("02") | L ("03") | L ("04") | L ("05") | L ("06") | L ("07") | L ("08") | L ("09") | L ("10") | L ("11") | L ("12") | L ("13") | L ("14") | L ("15") | L ("16") | L ("17") | L ("18") | L ("19") | L ("20") | L ("21") | L ("22") | L ("23");

			TimeMinute.Rule = 
				L ("0") + DecimalDigit
				 | L ("1") + DecimalDigit
				 | L ("2") + DecimalDigit
				 | L ("3") + DecimalDigit
				 | L ("4") + DecimalDigit
				 | L ("5") + DecimalDigit;

			TimeSecond.Rule = 
				L ("0") + DecimalDigit + Opt (TimeSecondDecimalPart)
				 | L ("1") + DecimalDigit + Opt (TimeSecondDecimalPart)
				 | L ("2") + DecimalDigit + Opt (TimeSecondDecimalPart)
				 | L ("3") + DecimalDigit + Opt (TimeSecondDecimalPart)
				 | L ("4") + DecimalDigit + Opt (TimeSecondDecimalPart)
				 | L ("5") + DecimalDigit + Opt (TimeSecondDecimalPart)
				 | L ("60") + Opt (TimeSecondDecimalPart);

			TimeSecondDecimalPart.Rule = 
				L (".") + DecimalDigits;

			TextLiteral.Rule = 
				L ("'") + SingleQuotedCharacters + L ("'")
				 | L ("\"") + DoubleQuotedCharacters + L ("\"")
				 | L ("@") + L ("'") + Opt (SingleQuotedVerbatimCharacters) + L ("'")
				 | L ("@") + L ("\"") + Opt (DoubleQuotedVerbatimCharacters) + L ("\"");

			SingleQuotedCharacters.Rule = 
				SingleQuotedCharacter
				 | SingleQuotedCharacters + SingleQuotedCharacter;

			SingleQuotedCharacter.Rule = 
				SingleQuotedCharacterSimple
				 | CharacterEscapeSimple
				 | CharacterEscapeUnicode;

			SingleQuotedCharacterSimple.Rule = 
				NoneOf (L ("'") | L ("\\") | NewLineCharacter);

			SingleQuotedVerbatimCharacters.Rule = 
				SingleQuotedVerbatimCharacter
				 | SingleQuotedVerbatimCharacters + SingleQuotedVerbatimCharacter;

			SingleQuotedVerbatimCharacter.Rule = 
				NoneOf (L ("'"))
				 | SingleQuotedVerbatimCharacterEscape;

			SingleQuotedVerbatimCharacterEscape.Rule = 
				L ("\"") + L ("\"");

			DoubleQuotedCharacter.Rule = 
				DoubleQuotedCharacterSimple
				 | CharacterEscape;

			DoubleQuotedCharacters.Rule = 
				DoubleQuotedCharacter
				 | DoubleQuotedCharacters + DoubleQuotedCharacter;

			DoubleQuotedCharacterSimple.Rule = 
				NoneOf (L ("\"") | L ("\\") | NewLineCharacter);

			DoubleQuotedVerbatimCharacter.Rule = 
				NoneOf (L ("\""))
				 | DoubleQuotedVerbatimCharacterEscape;

			DoubleQuotedVerbatimCharacters.Rule = 
				DoubleQuotedVerbatimCharacter
				 | DoubleQuotedVerbatimCharacters + DoubleQuotedVerbatimCharacter;

			DoubleQuotedVerbatimCharacterEscape.Rule = 
				L ("\"") + L ("\"");

			CharacterEscape.Rule = 
				CharacterEscapeSimple
				 | CharacterEscapeUnicode;

			CharacterEscapeSimple.Rule = 
				L ("\\") + CharacterEscapeSimpleCharacter;

			CharacterEscapeSimpleCharacter.Rule = 
				L ("'") | L ("\"") | L ("\\") | L ("0") | L ("a") | L ("b") | L ("f") | L ("n") | L ("r") | L ("t") | L ("v");

			CharacterEscapeUnicode.Rule = 
				L ("\\u") + HexDigit + HexDigit + HexDigit + HexDigit
				 | L ("\\U") + HexDigit + HexDigit + HexDigit + HexDigit + HexDigit + HexDigit + HexDigit + HexDigit;

			LogicalLiteral.Rule = 
				L ("true") | L ("false");

			BinaryLiteral.Rule = 
				L ("0x") + Opt (HexDigits)
				 | L ("0X") + Opt (HexDigits);

			HexDigit.Rule = 
				GetHexDigitToken();

			HexDigits.Rule = 
				HexDigit + HexDigit
				 | HexDigits + HexDigit + HexDigit;

			NullLiteral.Rule = 
				L ("null");

			GuidLiteral.Rule = 
				L ("#[") + X + X + X + X + X + X + X + X + L ("-") + X + X + X + X + L ("-") + X + X + X + X + L ("-") + X + X + X + X + L ("-") + X + X + X + X + X + X + X + X + X + X + X + X + L ("]");

			X.Rule = 
				HexDigit;

			OperatorOrPunctuator.Rule = 
				L ("[") | L ("]") | L ("(") | L (")") | L (".") | L (",") | L (":") | L (";") | L ("?") | L ("=") | L ("<") | L (">") | L ("<=") | L (">=") | L ("==") | L ("!=") | L ("+") | L ("-") | L ("*") | L ("/") | L ("%") | L ("&") | L ("|") | L ("!") | L ("&&") | L ("||") | L ("~") | L ("<<") | L (">>") | L ("{") | L ("}") | L ("#") | L ("..") | L ("@") | L ("'") | L ("\"") | L ("??");

			Identifiers.Rule = 
				Identifier
				 | Identifiers + L (",") + Identifier;

			Primary.Rule = 
				ReferencePrimary
				 | TextLiteral
				 | RepetitionPrimary
				 | CharacterClassPrimary
				 | InlineRulePrimary
				 | AnyPrimary;

			CharacterClassPrimary.Rule = 
				TextLiteral + L ("..") + TextLiteral;

			ReferencePrimary.Rule = 
				GrammarReference;

			GrammarReference.Rule = 
				Identifier
				 | GrammarReference + L (".") + Identifier
				 | GrammarReference + L (".") + Identifier + L ("(") + TypeArguments + L (")")
				 | Identifier + L ("(") + TypeArguments + L (")");

			TypeArguments.Rule = 
				PrimaryExpression
				 | TypeArguments + L (",") + PrimaryExpression;

			RepetitionPrimary.Rule = 
				Primary + Range
				 | Primary + CollectionRanges;

			Range.Rule = 
				L ("?")
				 | L ("*")
				 | L ("+");

			CollectionRanges.Rule = 
				L ("#") + IntegerLiteral
				 | L ("#") + IntegerLiteral + L ("..") + Opt (IntegerLiteral);

			InlineRulePrimary.Rule = 
				L ("(") + ProductionDeclarations + L (")");

			AnyPrimary.Rule = 
				L ("any");

			TextPatternExpression.Rule = 
				Difference;

			Difference.Rule = 
				Intersect
				 | Difference + L ("-") + Intersect;

			Intersect.Rule = 
				Inverse
				 | Intersect + L ("&") + Inverse;

			Inverse.Rule = 
				Primary
				 | L ("^") + Primary;

			ProductionDeclaration.Rule = 
				Opt (ProductionPrecedence) + PatternDeclaration + Opt (Constructor);

			Constructor.Rule = 
				L ("=>") + TermConstructor;

			ProductionPrecedence.Rule = 
				L ("precedence") + IntegerLiteral + L (":");

			PatternDeclaration.Rule = 
				L ("empty")
				 | Opt (TermDeclarations);

			TermDeclarations.Rule = 
				TermDeclaration
				 | TermDeclarations + TermDeclaration;

			TermDeclaration.Rule = 
				L ("error")
				 | Opt (Attributes) + Opt (TermPrecedence) + Opt (VariableBinding) + TextPatternExpression;

			VariableBinding.Rule = 
				Name + L (":");

			TermPrecedence.Rule = 
				L ("left") + L ("(") + IntegerLiteral + L (")")
				 | L ("right") + L ("(") + IntegerLiteral + L (")");

			TermConstructor.Rule = 
				TopLevelNode;

			Node.Rule = 
				Atom
				 | OrderedTerm
				 | UnorderedTerm;

			TopLevelNode.Rule = 
				TopLevelAtom
				 | OrderedTerm
				 | UnorderedTerm;

			Nodes.Rule = 
				Node
				 | Nodes + L (",") + Node;

			OrderedTerm.Rule = 
				Opt (Label) + L ("[") + Opt (Nodes) + L ("]");

			UnorderedTerm.Rule = 
				Opt (Label) + L ("{") + Opt (Nodes) + L ("}");

			Label.Rule = 
				Identifier
				 | L ("id") + L ("(") + Atom + L (")");

			Atom.Rule = 
				TopLevelAtom
				 | L ("valuesof") + L ("(") + VariableReference + L (")");

			TopLevelAtom.Rule = 
				TextLiteral
				 | DecimalLiteral
				 | LogicalLiteral
				 | IntegerLiteral
				 | NullLiteral
				 | VariableReference
				 | L ("labelof") + L ("(") + VariableReference + L (")");

			VariableReference.Rule = 
				Identifier;

			RuleDeclaration.Rule = 
				Opt (Attributes) + Opt (MemberModifier) + Kind + Name + Opt (RuleParameters) + Opt (RuleBody) + L (";");

			Kind.Rule = 
				L ("token")
				 | L ("syntax")
				 | L ("interleave");

			MemberModifier.Rule = 
				L ("final");

			RuleBody.Rule = 
				L ("=") + ProductionDeclarations;

			ProductionDeclarations.Rule = 
				ProductionDeclaration
				 | ProductionDeclarations + L ("|") + ProductionDeclaration;

			RuleParameters.Rule = 
				L ("(") + RuleParameterList + L (")");

			RuleParameterList.Rule = 
				RuleParameter
				 | RuleParameterList + L (",") + RuleParameter;

			RuleParameter.Rule = 
				Identifier;

			LanguageDeclaration.Rule = 
				Opt (Attributes) + L ("language") + Name + LanguageBody;

			LanguageBody.Rule = 
				L ("{") + Opt (RuleDeclarations) + L ("}");

			RuleDeclarations.Rule = 
				RuleDeclaration
				 | RuleDeclarations + RuleDeclaration;

			TypeDeclaration.Rule = 
				L ("type") + Identifier + L (";")
				 | L ("type") + Identifier + InitializationExpression + Opt (L (";"))
				 | L ("type") + Identifier + EntityTypeExpression + Opt (L (";"))
				 | L ("type") + Identifier + EntityTypeExpression + L ("where") + WhereExpressions + L (";")
				 | L ("type") + Identifier + L (":") + Expression + L (";")
				 | L ("type") + Identifier + L (":") + TypeReferences + L (";")
				 | L ("type") + Identifier + L (":") + TypeReferences + EntityTypeExpression + Opt (L (";"))
				 | L ("type") + Identifier + L (":") + TypeReferences + EntityTypeExpression + L ("where") + WhereExpressions;;

			TypeAscription.Rule = 
				TypeReference;

			TypeReference.Rule = 
				QualifiedIdentifier;

			TypeReferences.Rule = 
				TypeReference
				 | TypeReferences + L (",") + TypeReference;

			Pattern.Rule = 
				PatternElement
				 | Pattern + PatternElement;

			PatternElement.Rule = 
				NormalCharacter
				 | L ("-")
				 | L ("%")
				 | L ("[") + NormalCharacter + L ("-") + NormalCharacter + L ("]")
				 | L ("[^") + NormalCharacter + L ("-") + NormalCharacter + L ("]");

			EntityTypeExpression.Rule = 
				L ("{") + EntityMemberDeclarations + L ("}");

			EntityMemberDeclarations.Rule = 
				EntityMemberDeclaration
				 | EntityMemberDeclarations + EntityMemberDeclaration;

			EntityMemberDeclaration.Rule = 
				FieldDeclaration
				 | ComputedValueDeclaration;

			ComputedValueDeclaration.Rule = 
				Identifier + FormalParameters + Opt (ReturnType) + ExpressionBody
				 | L ("extern") + Identifier + FormalParameters + ReturnType + L (";");

			FormalParameters.Rule = 
				L ("(") + Opt (Parameters) + L (")");

			Parameters.Rule = 
				Parameter
				 | Parameters + L (",") + Parameter;

			Parameter.Rule = 
				Identifier + Opt (TypeAscription);

			ReturnType.Rule = 
				TypeAscription;

			ExpressionBody.Rule = 
				L ("{") + Expression + L (";") + L ("}");

			FieldDeclaration.Rule = 
				DottedIdentifier + Opt (TypeAscription) + L ("=") + Expression + L (";")
				 | DottedIdentifier + Opt (TypeAscription) + InitializationExpression + Opt (L (";"))
				 | DottedIdentifiers + Opt (TypeAscription) + L (";")
				 | L ("extern") + Identifier + TypeAscription + L (";");

			DottedIdentifiers.Rule = 
				DottedIdentifier
				 | DottedIdentifiers + L (",") + DottedIdentifier;

			DottedIdentifier.Rule = 
				IdentifierPath
				 | L (".") + IdentifierPath;

			IdentifierPath.Rule = 
				Identifier
				 | IdentifierPath + L (".") + Identifier;

			MemberAccessExpression.Rule = 
				PrimaryExpression + L (".") + MemberName;

			MemberName.Rule = 
				Identifier;

			InitializationExpression.Rule = 
				L ("{") + Opt (ElementInitializers) + L ("}")
				 | L ("{") + ElementInitializers + L (",") + L ("}");

			ElementInitializer.Rule = 
				LeadingDottedIdentifier + Opt (TypeAscription) + L ("=") + Expression
				 | LeadingDottedIdentifier + Opt (TypeAscription) + InitializationExpression;

			ElementInitializers.Rule = 
				ElementInitializer
				 | ElementInitializers + L (",") + ElementInitializer;

			LeadingDottedIdentifier.Rule = 
				DottedIdentifier
				 | L (".") + DottedIdentifier;

			InvocationExpression.Rule = 
				Identifier + InvocationExpressionArguments;

			InvocationExpressionArguments.Rule = 
				L ("(") + Opt (Arguments) + L (")");

			Arguments.Rule = 
				Argument
				 | Arguments + L (",") + Argument;

			Argument.Rule = 
				Expression;

			PrimaryExpression.Rule = 
				PrimaryCreationExpression;

			PrimaryCreationExpression.Rule = 
				Literal
				 | SimpleName
				 | ParenthesizedExpression
				 | MemberAccessExpression
				 | InvocationExpression
				 | InitializationExpression
				 | EntityTypeExpression
				 | ContextVariable
				 | InfoOfExpression;

			Name.Rule = 
				SimpleName;

			SimpleName.Rule = 
				Identifier;

			ParenthesizedExpression.Rule = 
				L ("(") + Expression + L (")");

			ContextVariable.Rule = 
				L ("value");

			InfoOfExpression.Rule = 
				L ("infoof") + L ("(") + DottedIdentifier + L (")");

			UnaryExpression.Rule = 
				PrimaryExpression
				 | L ("+") + PrimaryExpression
				 | L ("-") + PrimaryExpression
				 | L ("!") + PrimaryExpression
				 | L ("~") + PrimaryExpression
				 | PrimaryExpression + L ("#")
				 | IdentityExpression
				 | UniqueExpression;

			IdentityExpression.Rule = 
				L ("identity") + Identifier
				 | L ("identity") + L ("(") + Identifiers + L (")");

			UniqueExpression.Rule = 
				L ("unique") + Identifier
				 | L ("unique") + L ("(") + Identifiers + L (")");

			MultiplicityExpression.Rule = 
				UnaryExpression
				 | UnaryExpression + L ("?")
				 | UnaryExpression + L ("+")
				 | UnaryExpression + L ("*")
				 | UnaryExpression + L ("#") + IntegralRange;

			IntegralRange.Rule = 
				IntegerLiteral
				 | IntegerLiteral + L ("..")
				 | IntegerLiteral + L ("..") + IntegerLiteral;

			AdditiveExpression.Rule = 
				MultiplicativeExpression
				 | AdditiveExpression + L ("+") + MultiplicativeExpression
				 | AdditiveExpression + L ("-") + MultiplicativeExpression;

			MultiplicativeExpression.Rule = 
				UnaryExpression
				 | MultiplicativeExpression + L ("*") + MultiplicityExpression
				 | MultiplicativeExpression + L ("/") + MultiplicityExpression
				 | MultiplicativeExpression + L ("%") + MultiplicityExpression;

			ShiftExpression.Rule = 
				AdditiveExpression
				 | ShiftExpression + L ("<<") + AdditiveExpression
				 | ShiftExpression + L (">>") + AdditiveExpression;

			RelationalExpression.Rule = 
				ShiftExpression
				 | RelationalExpression + L ("<") + ShiftExpression
				 | RelationalExpression + L (">") + ShiftExpression
				 | RelationalExpression + L ("<=") + ShiftExpression
				 | RelationalExpression + L (">=") + ShiftExpression
				 | RelationalExpression + L ("in") + ShiftExpression
				 | RelationalExpression + L (":") + ShiftExpression;

			EqualityExpression.Rule = 
				RelationalExpression
				 | EqualityExpression + L ("==") + RelationalExpression
				 | EqualityExpression + L ("!=") + RelationalExpression;

			LogicalAndExpression.Rule = 
				EqualityExpression
				 | LogicalAndExpression + L ("&&") + EqualityExpression;

			LogicalOrExpression.Rule = 
				LogicalAndExpression
				 | LogicalOrExpression + L ("||") + LogicalAndExpression;

			NullCoalescingExpression.Rule = 
				LogicalOrExpression
				 | LogicalOrExpression + L ("??") + NullCoalescingExpression;

			ConditionalExpression.Rule = 
				NullCoalescingExpression
				 | NullCoalescingExpression + L ("?") + Expression + L (":") + Expression;

			QueryExpression.Rule = 
				ConditionalExpression
				 | QueryFromClause + QueryBody;

			QueryBody.Rule = 
				Opt (QueryBodyClauses) + QueryConstructor;

			QueryBodyClauses.Rule = 
				QueryBodyClause
				 | QueryBodyClauses + QueryBodyClause;

			QueryBodyClause.Rule = 
				QueryFromClause
				 | QueryLetClause
				 | QueryWhereClause
				 | QueryJoinClause;

			QueryConstructor.Rule = 
				QuerySelectClause
				 | QueryGroupClause
				 | QueryAccumulateClause;

			QueryFromClause.Rule = 
				L ("from") + Identifier + L ("in") + ConditionalExpression;

			QueryLetClause.Rule = 
				L ("let") + Identifier + L ("=") + ConditionalExpression;

			QueryJoinClause.Rule = 
				L ("join") + Identifier + L ("in") + Expression + L ("on") + Expression + L ("equals") + ConditionalExpression;

			QueryWhereClause.Rule = 
				L ("where") + ConditionalExpression;

			QuerySelectClause.Rule = 
				L ("select") + ConditionalExpression;

			QueryGroupClause.Rule = 
				L ("group") + Expression + L ("by") + ConditionalExpression;

			QueryAccumulateClause.Rule = 
				QueryLetClause + L ("accumulate") + ConditionalExpression;

			WhereExpression.Rule = 
				QueryExpression
				 | QueryExpression + L ("where") + WhereExpressions;

			WhereExpressions.Rule = 
				WhereExpression
				 | WhereExpressions + L (",") + WhereExpression;

			SelectExpression.Rule = 
				WhereExpression
				 | WhereExpression + L ("select") + Expression;

			InclusiveOrExpression.Rule = 
				ExclusiveOrExpression
				 | InclusiveOrExpression + L ("|") + ExclusiveOrExpression;

			ExclusiveOrExpression.Rule = 
				AndExpression
				 | ExclusiveOrExpression + L ("^") + AndExpression;

			AndExpression.Rule = 
				SelectExpression
				 | AndExpression + L ("&") + SelectExpression;

			Expression.Rule = 
				InclusiveOrExpression;

			CompilationUnit.Rule = 
				ModuleDeclarationList;

			ModuleDeclarationList.Rule = 
				ModuleDeclaration
				 | ModuleDeclarationList + ModuleDeclaration;

			ModuleDeclaration.Rule = 
				L ("module") + QualifiedIdentifier + ModuleBody + Opt (L (";"));

			QualifiedIdentifier.Rule = 
				Identifier
				 | QualifiedIdentifier + L (".") + Identifier;

			ModuleBody.Rule = 
				L ("{") + ImportDirectives + ExportDirectives + ModuleMemberDeclarations + L ("}");

			ModuleMemberDeclarations.Rule = 
				L ("empty")
				 | ModuleMemberDeclarations + ModuleMemberDeclaration;

			ModuleMemberDeclaration.Rule = 
				LanguageDeclaration
				 | FieldDeclaration
				 | ComputedValueDeclaration
				 | TypeDeclaration;

			ExportDirectives.Rule = 
				L ("empty")
				 | ExportDirectives + ExportDirective;

			ExportDirective.Rule = 
				L ("export") + Identifiers;;

			ImportDirectives.Rule = 
				L ("empty")
				 | ImportDirectives + ImportDirective;

			ImportDirective.Rule = 
				L ("import") + ImportModules + L (";")
				 | L ("import") + QualifiedIdentifier + L ("{") + ImportMembers + L ("}") + L (";");

			ImportMember.Rule = 
				Identifier + Opt (ImportAlias);

			ImportMembers.Rule = 
				ImportMember
				 | ImportMembers + L (",") + ImportMember;

			ImportModule.Rule = 
				QualifiedIdentifier + Opt (ImportAlias);

			ImportModules.Rule = 
				ImportModule
				 | ImportModules + L (",") + ImportModule;

			ImportAlias.Rule = 
				L ("as") + Identifier;

			Attributes.Rule = 
				AttributeSections;

			AttributeSections.Rule = 
				AttributeSection
				 | AttributeSections + AttributeSection;

			AttributeSection.Rule = 
				L ("@{") + Nodes + L ("}");


		}

		public StringLiteral L (string s)
		{
			return new StringLiteral (s);
		}

		public NonTerminal Opt (BnfTerm elem)
		{
			return new NonTerminal (elem.Name + ".opt") { Rule = Empty + elem };
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
		StringLiteral T { get { return new StringLiteral ("T"); } }
		StringLiteral Z { get { return new StringLiteral ("Z"); } }
		StringLiteral E { get { return new StringLiteral ("E"); } }

		// ---- Non-terminals -----
		NonTerminal NormalCharacter = new NonTerminal ("NormalCharacter");
		NonTerminal Input = new NonTerminal ("Input");
		NonTerminal InputSection = new NonTerminal ("InputSection");
		NonTerminal InputSectionPart = new NonTerminal ("InputSectionPart");
		NonTerminal InputElements = new NonTerminal ("InputElements");
		NonTerminal InputElement = new NonTerminal ("InputElement");
		NonTerminal NewLine = new NonTerminal ("NewLine");
		NonTerminal NewLineCharacter = new NonTerminal ("NewLineCharacter");
		NonTerminal Whitespace = new NonTerminal ("Whitespace");
		NonTerminal WhitespaceCharacter = new NonTerminal ("WhitespaceCharacter");
		NonTerminal WhitespaceCharacters = new NonTerminal ("WhitespaceCharacters");
		NonTerminal Token = new NonTerminal ("Token");
		NonTerminal Identifier = new NonTerminal ("Identifier");
		NonTerminal IdentifierBegin = new NonTerminal ("IdentifierBegin");
		NonTerminal IdentifierCharacter = new NonTerminal ("IdentifierCharacter");
		NonTerminal IdentifierCharacters = new NonTerminal ("IdentifierCharacters");
		NonTerminal IdentifierVerbatim = new NonTerminal ("IdentifierVerbatim");
		NonTerminal IdentifierVerbatimCharacter = new NonTerminal ("IdentifierVerbatimCharacter");
		NonTerminal IdentifierVerbatimCharacters = new NonTerminal ("IdentifierVerbatimCharacters");
		NonTerminal IdentifierVerbatimEscape = new NonTerminal ("IdentifierVerbatimEscape");
		NonTerminal Letter = new NonTerminal ("Letter");
		NonTerminal DecimalDigit = new NonTerminal ("DecimalDigit");
		NonTerminal DecimalDigits = new NonTerminal ("DecimalDigits");
		NonTerminal Keyword = new NonTerminal ("Keyword");
		NonTerminal Literal = new NonTerminal ("Literal");
		NonTerminal DecimalLiteral = new NonTerminal ("DecimalLiteral");
		NonTerminal IntegerLiteral = new NonTerminal ("IntegerLiteral");
		NonTerminal ScientificLiteral = new NonTerminal ("ScientificLiteral");
		NonTerminal Sign = new NonTerminal ("Sign");
		NonTerminal DateLiteral = new NonTerminal ("DateLiteral");
		NonTerminal DateDay = new NonTerminal ("DateDay");
		NonTerminal DateMonth = new NonTerminal ("DateMonth");
		NonTerminal DateYear = new NonTerminal ("DateYear");
		NonTerminal DateTimeLiteral = new NonTerminal ("DateTimeLiteral");
		NonTerminal DateTimeOffsetLiteral = new NonTerminal ("DateTimeOffsetLiteral");
		NonTerminal TimeZoneLiteral = new NonTerminal ("TimeZoneLiteral");
		NonTerminal TimeLiteral = new NonTerminal ("TimeLiteral");
		NonTerminal TimeHourMinute = new NonTerminal ("TimeHourMinute");
		NonTerminal TimeHour = new NonTerminal ("TimeHour");
		NonTerminal TimeMinute = new NonTerminal ("TimeMinute");
		NonTerminal TimeSecond = new NonTerminal ("TimeSecond");
		NonTerminal TimeSecondDecimalPart = new NonTerminal ("TimeSecondDecimalPart");
		NonTerminal TextLiteral = new NonTerminal ("TextLiteral");
		NonTerminal SingleQuotedCharacters = new NonTerminal ("SingleQuotedCharacters");
		NonTerminal SingleQuotedCharacter = new NonTerminal ("SingleQuotedCharacter");
		NonTerminal SingleQuotedCharacterSimple = new NonTerminal ("SingleQuotedCharacterSimple");
		NonTerminal SingleQuotedVerbatimCharacters = new NonTerminal ("SingleQuotedVerbatimCharacters");
		NonTerminal SingleQuotedVerbatimCharacter = new NonTerminal ("SingleQuotedVerbatimCharacter");
		NonTerminal SingleQuotedVerbatimCharacterEscape = new NonTerminal ("SingleQuotedVerbatimCharacterEscape");
		NonTerminal DoubleQuotedCharacter = new NonTerminal ("DoubleQuotedCharacter");
		NonTerminal DoubleQuotedCharacters = new NonTerminal ("DoubleQuotedCharacters");
		NonTerminal DoubleQuotedCharacterSimple = new NonTerminal ("DoubleQuotedCharacterSimple");
		NonTerminal DoubleQuotedVerbatimCharacter = new NonTerminal ("DoubleQuotedVerbatimCharacter");
		NonTerminal DoubleQuotedVerbatimCharacters = new NonTerminal ("DoubleQuotedVerbatimCharacters");
		NonTerminal DoubleQuotedVerbatimCharacterEscape = new NonTerminal ("DoubleQuotedVerbatimCharacterEscape");
		NonTerminal CharacterEscape = new NonTerminal ("CharacterEscape");
		NonTerminal CharacterEscapeSimple = new NonTerminal ("CharacterEscapeSimple");
		NonTerminal CharacterEscapeSimpleCharacter = new NonTerminal ("CharacterEscapeSimpleCharacter");
		NonTerminal CharacterEscapeUnicode = new NonTerminal ("CharacterEscapeUnicode");
		NonTerminal LogicalLiteral = new NonTerminal ("LogicalLiteral");
		NonTerminal BinaryLiteral = new NonTerminal ("BinaryLiteral");
		NonTerminal HexDigit = new NonTerminal ("HexDigit");
		NonTerminal HexDigits = new NonTerminal ("HexDigits");
		NonTerminal NullLiteral = new NonTerminal ("NullLiteral");
		NonTerminal GuidLiteral = new NonTerminal ("GuidLiteral");
		NonTerminal X = new NonTerminal ("X");
		NonTerminal OperatorOrPunctuator = new NonTerminal ("OperatorOrPunctuator");
		NonTerminal Identifiers = new NonTerminal ("Identifiers");
		NonTerminal Primary = new NonTerminal ("Primary");
		NonTerminal CharacterClassPrimary = new NonTerminal ("CharacterClassPrimary");
		NonTerminal ReferencePrimary = new NonTerminal ("ReferencePrimary");
		NonTerminal GrammarReference = new NonTerminal ("GrammarReference");
		NonTerminal TypeArguments = new NonTerminal ("TypeArguments");
		NonTerminal RepetitionPrimary = new NonTerminal ("RepetitionPrimary");
		NonTerminal Range = new NonTerminal ("Range");
		NonTerminal CollectionRanges = new NonTerminal ("CollectionRanges");
		NonTerminal InlineRulePrimary = new NonTerminal ("InlineRulePrimary");
		NonTerminal AnyPrimary = new NonTerminal ("AnyPrimary");
		NonTerminal TextPatternExpression = new NonTerminal ("TextPatternExpression");
		NonTerminal Difference = new NonTerminal ("Difference");
		NonTerminal Intersect = new NonTerminal ("Intersect");
		NonTerminal Inverse = new NonTerminal ("Inverse");
		NonTerminal ProductionDeclaration = new NonTerminal ("ProductionDeclaration");
		NonTerminal Constructor = new NonTerminal ("Constructor");
		NonTerminal ProductionPrecedence = new NonTerminal ("ProductionPrecedence");
		NonTerminal PatternDeclaration = new NonTerminal ("PatternDeclaration");
		NonTerminal TermDeclarations = new NonTerminal ("TermDeclarations");
		NonTerminal TermDeclaration = new NonTerminal ("TermDeclaration");
		NonTerminal VariableBinding = new NonTerminal ("VariableBinding");
		NonTerminal TermPrecedence = new NonTerminal ("TermPrecedence");
		NonTerminal TermConstructor = new NonTerminal ("TermConstructor");
		NonTerminal Node = new NonTerminal ("Node");
		NonTerminal TopLevelNode = new NonTerminal ("TopLevelNode");
		NonTerminal Nodes = new NonTerminal ("Nodes");
		NonTerminal OrderedTerm = new NonTerminal ("OrderedTerm");
		NonTerminal UnorderedTerm = new NonTerminal ("UnorderedTerm");
		NonTerminal Label = new NonTerminal ("Label");
		NonTerminal Atom = new NonTerminal ("Atom");
		NonTerminal TopLevelAtom = new NonTerminal ("TopLevelAtom");
		NonTerminal VariableReference = new NonTerminal ("VariableReference");
		NonTerminal RuleDeclaration = new NonTerminal ("RuleDeclaration");
		NonTerminal Kind = new NonTerminal ("Kind");
		NonTerminal MemberModifier = new NonTerminal ("MemberModifier");
		NonTerminal RuleBody = new NonTerminal ("RuleBody");
		NonTerminal ProductionDeclarations = new NonTerminal ("ProductionDeclarations");
		NonTerminal RuleParameters = new NonTerminal ("RuleParameters");
		NonTerminal RuleParameterList = new NonTerminal ("RuleParameterList");
		NonTerminal RuleParameter = new NonTerminal ("RuleParameter");
		NonTerminal LanguageDeclaration = new NonTerminal ("LanguageDeclaration");
		NonTerminal LanguageBody = new NonTerminal ("LanguageBody");
		NonTerminal RuleDeclarations = new NonTerminal ("RuleDeclarations");
		NonTerminal TypeDeclaration = new NonTerminal ("TypeDeclaration");
		NonTerminal TypeAscription = new NonTerminal ("TypeAscription");
		NonTerminal TypeReference = new NonTerminal ("TypeReference");
		NonTerminal TypeReferences = new NonTerminal ("TypeReferences");
		NonTerminal Pattern = new NonTerminal ("Pattern");
		NonTerminal PatternElement = new NonTerminal ("PatternElement");
		NonTerminal EntityTypeExpression = new NonTerminal ("EntityTypeExpression");
		NonTerminal EntityMemberDeclarations = new NonTerminal ("EntityMemberDeclarations");
		NonTerminal EntityMemberDeclaration = new NonTerminal ("EntityMemberDeclaration");
		NonTerminal ComputedValueDeclaration = new NonTerminal ("ComputedValueDeclaration");
		NonTerminal FormalParameters = new NonTerminal ("FormalParameters");
		NonTerminal Parameters = new NonTerminal ("Parameters");
		NonTerminal Parameter = new NonTerminal ("Parameter");
		NonTerminal ReturnType = new NonTerminal ("ReturnType");
		NonTerminal ExpressionBody = new NonTerminal ("ExpressionBody");
		NonTerminal FieldDeclaration = new NonTerminal ("FieldDeclaration");
		NonTerminal DottedIdentifiers = new NonTerminal ("DottedIdentifiers");
		NonTerminal DottedIdentifier = new NonTerminal ("DottedIdentifier");
		NonTerminal IdentifierPath = new NonTerminal ("IdentifierPath");
		NonTerminal MemberAccessExpression = new NonTerminal ("MemberAccessExpression");
		NonTerminal MemberName = new NonTerminal ("MemberName");
		NonTerminal InitializationExpression = new NonTerminal ("InitializationExpression");
		NonTerminal ElementInitializer = new NonTerminal ("ElementInitializer");
		NonTerminal ElementInitializers = new NonTerminal ("ElementInitializers");
		NonTerminal LeadingDottedIdentifier = new NonTerminal ("LeadingDottedIdentifier");
		NonTerminal InvocationExpression = new NonTerminal ("InvocationExpression");
		NonTerminal InvocationExpressionArguments = new NonTerminal ("InvocationExpressionArguments");
		NonTerminal Arguments = new NonTerminal ("Arguments");
		NonTerminal Argument = new NonTerminal ("Argument");
		NonTerminal PrimaryExpression = new NonTerminal ("PrimaryExpression");
		NonTerminal PrimaryCreationExpression = new NonTerminal ("PrimaryCreationExpression");
		NonTerminal Name = new NonTerminal ("Name");
		NonTerminal SimpleName = new NonTerminal ("SimpleName");
		NonTerminal ParenthesizedExpression = new NonTerminal ("ParenthesizedExpression");
		NonTerminal ContextVariable = new NonTerminal ("ContextVariable");
		NonTerminal InfoOfExpression = new NonTerminal ("InfoOfExpression");
		NonTerminal UnaryExpression = new NonTerminal ("UnaryExpression");
		NonTerminal IdentityExpression = new NonTerminal ("IdentityExpression");
		NonTerminal UniqueExpression = new NonTerminal ("UniqueExpression");
		NonTerminal MultiplicityExpression = new NonTerminal ("MultiplicityExpression");
		NonTerminal IntegralRange = new NonTerminal ("IntegralRange");
		NonTerminal AdditiveExpression = new NonTerminal ("AdditiveExpression");
		NonTerminal MultiplicativeExpression = new NonTerminal ("MultiplicativeExpression");
		NonTerminal ShiftExpression = new NonTerminal ("ShiftExpression");
		NonTerminal RelationalExpression = new NonTerminal ("RelationalExpression");
		NonTerminal EqualityExpression = new NonTerminal ("EqualityExpression");
		NonTerminal LogicalAndExpression = new NonTerminal ("LogicalAndExpression");
		NonTerminal LogicalOrExpression = new NonTerminal ("LogicalOrExpression");
		NonTerminal NullCoalescingExpression = new NonTerminal ("NullCoalescingExpression");
		NonTerminal ConditionalExpression = new NonTerminal ("ConditionalExpression");
		NonTerminal QueryExpression = new NonTerminal ("QueryExpression");
		NonTerminal QueryBody = new NonTerminal ("QueryBody");
		NonTerminal QueryBodyClauses = new NonTerminal ("QueryBodyClauses");
		NonTerminal QueryBodyClause = new NonTerminal ("QueryBodyClause");
		NonTerminal QueryConstructor = new NonTerminal ("QueryConstructor");
		NonTerminal QueryFromClause = new NonTerminal ("QueryFromClause");
		NonTerminal QueryLetClause = new NonTerminal ("QueryLetClause");
		NonTerminal QueryJoinClause = new NonTerminal ("QueryJoinClause");
		NonTerminal QueryWhereClause = new NonTerminal ("QueryWhereClause");
		NonTerminal QuerySelectClause = new NonTerminal ("QuerySelectClause");
		NonTerminal QueryGroupClause = new NonTerminal ("QueryGroupClause");
		NonTerminal QueryAccumulateClause = new NonTerminal ("QueryAccumulateClause");
		NonTerminal WhereExpression = new NonTerminal ("WhereExpression");
		NonTerminal WhereExpressions = new NonTerminal ("WhereExpressions");
		NonTerminal SelectExpression = new NonTerminal ("SelectExpression");
		NonTerminal InclusiveOrExpression = new NonTerminal ("InclusiveOrExpression");
		NonTerminal ExclusiveOrExpression = new NonTerminal ("ExclusiveOrExpression");
		NonTerminal AndExpression = new NonTerminal ("AndExpression");
		NonTerminal Expression = new NonTerminal ("Expression");
		NonTerminal CompilationUnit = new NonTerminal ("CompilationUnit");
		NonTerminal ModuleDeclarationList = new NonTerminal ("ModuleDeclarationList");
		NonTerminal ModuleDeclaration = new NonTerminal ("ModuleDeclaration");
		NonTerminal QualifiedIdentifier = new NonTerminal ("QualifiedIdentifier");
		NonTerminal ModuleBody = new NonTerminal ("ModuleBody");
		NonTerminal ModuleMemberDeclarations = new NonTerminal ("ModuleMemberDeclarations");
		NonTerminal ModuleMemberDeclaration = new NonTerminal ("ModuleMemberDeclaration");
		NonTerminal ExportDirectives = new NonTerminal ("ExportDirectives");
		NonTerminal ExportDirective = new NonTerminal ("ExportDirective");
		NonTerminal ImportDirectives = new NonTerminal ("ImportDirectives");
		NonTerminal ImportDirective = new NonTerminal ("ImportDirective");
		NonTerminal ImportMember = new NonTerminal ("ImportMember");
		NonTerminal ImportMembers = new NonTerminal ("ImportMembers");
		NonTerminal ImportModule = new NonTerminal ("ImportModule");
		NonTerminal ImportModules = new NonTerminal ("ImportModules");
		NonTerminal ImportAlias = new NonTerminal ("ImportAlias");
		NonTerminal Attributes = new NonTerminal ("Attributes");
		NonTerminal AttributeSections = new NonTerminal ("AttributeSections");
		NonTerminal AttributeSection = new NonTerminal ("AttributeSection");

		// ---- End of non-terminals ----
	}
}
