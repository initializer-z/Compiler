
//**************************************************************
//
// Code generator SKELETON
// name:朱祥辉 sno:518051910018
// cite:祁山青
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
#include <stack>
#include <queue>

using namespace std;

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

static char *CALL_REGS[] = {RDI, RSI, RDX, RCX, R8, R9};
static char *CALL_XMM[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5};

void cgen_helper(Decls decls, ostream& s);
void code(Decls decls, ostream& s);

//////////////////////////////////////////////////////////////////
//
//
//    Helper Functions
//  
//
//////////////////////////////////////////////////////////////////

// you can add any helper functions here


//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
    Int,
    Float,
    String,
    Bool,
    Void,
    Main,
    print
    ;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    // 4 basic types and Void type
    Bool        = idtable.add_string("Bool");
    Int         = idtable.add_string("Int");
    String      = idtable.add_string("String");
    Float       = idtable.add_string("Float");
    Void        = idtable.add_string("Void");  
    // main function
    Main        = idtable.add_string("main");

    // classical function to print things, so defined here for call.
    print        = idtable.add_string("printf");
}


//*********************************************************
//
// Define method for code generation
//
//
//*********************************************************

void Program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  cgen_helper(decls,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_mov(const char *source, const char *dest, ostream& s)
{
  s << MOV << source << COMMA << dest << endl;
}

static void emit_rmmov(const char *source_reg, int offset, const char *base_reg, ostream& s)
{
  s << MOV << source_reg << COMMA << offset << "(" << base_reg << ")"
      << endl;
}

static void emit_mrmov(const char *base_reg, int offset, const char *dest_reg, ostream& s)
{
  s << MOV << offset << "(" << base_reg << ")" << COMMA << dest_reg  
      << endl;
}

static void emit_irmov(const char *immidiate, const char *dest_reg, ostream& s)
{
  s << MOV << "$" << immidiate << COMMA << dest_reg  
      << endl;
}

static void emit_irmovl(const char *immidiate, const char *dest_reg, ostream& s)
{
  s << MOVL << "$" << immidiate << COMMA << dest_reg  
      << endl;
}

static void emit_immov(const char *immidiate, int offset, const char *base_reg, ostream& s)
{
  s << MOV << "$" << immidiate << COMMA << "(" << offset << ")" << base_reg  
      << endl;
}

static void emit_add(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << ADD << source_reg << COMMA << dest_reg << endl;
}

static void emit_sub(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << SUB << source_reg << COMMA << dest_reg << endl;
}

static void emit_mul(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << MUL << source_reg << COMMA << dest_reg << endl;
}

static void emit_div(const char *dest_reg, ostream& s)
{
  s << DIV << dest_reg << endl;
}

static void emit_cqto(ostream &s)
{
  s << CQTO << endl;
}

static void emit_neg(const char *dest_reg, ostream& s)
{
  s << NEG << dest_reg << endl;
}

static void emit_and(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << AND << source_reg << COMMA << dest_reg << endl;
}

static void emit_or(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << OR << source_reg << COMMA << dest_reg << endl;
}

static void emit_xor(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << XOR << source_reg << COMMA << dest_reg << endl;
}

static void emit_not(const char *dest_reg, ostream& s)
{
  s << NOT << " " << dest_reg << endl;
}

static void emit_movsd(const char *source, const char *dest, ostream& s)
{
  s << MOVSD << source << COMMA << dest << endl;
}

static void emit_movaps(const char *source, const char *dest, ostream& s)
{
  s << MOVAPS << source << COMMA << dest << endl;
}

static void emit_addsd(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << ADDSD << source_reg << COMMA << dest_reg << endl;
}

static void emit_subsd(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << SUBSD << source_reg << COMMA << dest_reg << endl;
}

static void emit_mulsd(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << MULSD << source_reg << COMMA << dest_reg << endl;
}

static void emit_divsd(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << DIVSD << source_reg << COMMA << dest_reg << endl;
}

static void emit_cmp(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << CMP << source_reg << COMMA << dest_reg << endl;
}

static void emit_test(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << TEST << source_reg << COMMA << dest_reg << endl;
}

static void emit_ucompisd(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << UCOMPISD << source_reg << COMMA << dest_reg << endl;
}

static void emit_xorpd(const char *source_reg, const char *dest_reg, ostream& s)
{
  s << XORPD << source_reg << COMMA << dest_reg << endl;
}
static void emit_jmp(const char *dest, ostream& s)
{
  s << JMP << " " << dest << endl;
}

static void emit_jl(const char *dest, ostream& s)
{
  s << JL << " " << dest << endl;
}

static void emit_jle(const char *dest, ostream& s)
{
  s << JLE << " " << dest << endl;
}

static void emit_je(const char *dest, ostream& s)
{
  s << JE << " " << dest << endl;
}

static void emit_jne(const char *dest, ostream& s)
{
  s << JNE << " " << dest << endl;
}

static void emit_jg(const char *dest, ostream& s)
{
  s << JG << " " << dest << endl;
}

static void emit_jge(const char *dest, ostream& s)
{
  s << JGE << " " << dest << endl;
}

static void emit_jb(const char *dest, ostream& s)
{
  s << JB << " " << dest << endl;
}

static void emit_jbe(const char *dest, ostream& s)
{
  s << JBE << " " << dest << endl;
}

static void emit_ja(const char *dest, ostream& s)
{
  s << JA << " " << dest << endl;
}

static void emit_jae(const char *dest, ostream& s)
{
  s << JAE << " " << dest << endl;
}

static void emit_jp(const char *dest, ostream& s)
{
  s << JP << " " << dest << endl;
}

static void emit_jz(const char *dest, ostream& s)
{
  s << JZ << " " << dest << endl;
}

static void emit_jnz(const char *dest, ostream& s)
{
  s << JNZ << " " << dest << endl;
}

static void emit_call(const char *dest, ostream& s)
{
  s << CALL << " " << dest << endl;
}

static void emit_ret(ostream& s)
{
  s << RET << endl;
}

static void emit_push(const char *reg, ostream& s)
{
  s << PUSH << " " << reg << endl;
}

static void emit_pop(const char *reg, ostream& s)
{
  s << POP << " " << reg << endl;
}

static void emit_leave(ostream& s)
{
  s << LEAVE << endl;
}

static void emit_position(const char *p, ostream& s)
{
  s << p << ":" << endl;
}

static void emit_float_to_int(const char *float_mmx, const char *int_reg, ostream& s)
{
  s << CVTTSD2SIQ << float_mmx << COMMA << int_reg << endl;
}

static void emit_int_to_float(const char *int_reg, const char *float_mmx, ostream& s)
{
  s << CVTSI2SDQ << int_reg << COMMA << float_mmx << endl;
}

void EmitPushes(ostream &s){
  emit_push(RBP,s);
  emit_mov(RSP,RBP,s);
  emit_push(RBX,s);
  emit_push(R10,s);
  emit_push(R11,s);
  emit_push(R12,s);
  emit_push(R13,s);
  emit_push(R14,s);
  emit_push(R15,s);
}

void EmitPops(ostream &s){
  emit_pop(R15,s);
  emit_pop(R14,s);
  emit_pop(R13,s);
  emit_pop(R12,s);
  emit_pop(R11,s);
  emit_pop(R10,s);
  emit_pop(RBX,s);
  s<<LEAVE<<endl<<
  RET<<endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Seal has four kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// If you like, you can add any ***Entry::code_def() and ***Entry::code_ref()
// functions to help.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << "$" << STRINGCONST_PREFIX << index;
}

//
// Emit code for a constant String.
//

void StringEntry::code_def(ostream& s)
{
  s << STRINGCONST_PREFIX << index << ":" << endl;
  s  << STRINGTAG ; emit_string_constant(s,str);                                                // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
map<Symbol,Symbol> CallType;
struct callTable
{
  int cur_length = 0;
  bool is_flt = 0;
  bool is_glb = 0;
  Symbol name; 
};
//将字符串按顺序输出
stack<StringEntry*> StrStack;

void StrTable::code_string_table(ostream& s)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    StrStack.push(l->hd());
  for(;!StrStack.empty();StrStack.pop()){
    StrStack.top()->code_def(s);
  }
    // l->hd()->code_def(s);
}

// the following 2 functions are useless, please DO NOT care about them
void FloatEntry::code_ref(ostream &s)
{
  s << FLOATTAG << index;
}

void IntEntry::code_def(ostream &s)
{
  s << GLOBAL;
}

//***************************************************
//
//  Emit global var and functions.
//
//***************************************************

static void emit_global_int(Symbol name, ostream& s) {
  s << GLOBAL << name << endl << 
  ALIGN << 8 << endl << 
  SYMBOL_TYPE << name << COMMA << OBJECT << endl <<
  SIZE << name << COMMA << 8 << endl << 
  name << ":" << endl << 
  INTTAG << 0 << endl;
}

static void emit_global_float(Symbol name, ostream& s) {
  s << GLOBAL << name << endl << 
  ALIGN << 8 << endl << 
  SYMBOL_TYPE << name << COMMA << OBJECT << endl <<
  SIZE << name << COMMA << 8 << endl << 
  name << ":" << endl <<
  FLOATTAG << 0 << endl <<
  FLOATTAG << 0 << endl;
}

static void emit_global_bool(Symbol name, ostream& s) {
  s << GLOBAL << name << endl << 
  ALIGN << 8 << endl << 
  SYMBOL_TYPE << name << COMMA << OBJECT << endl <<
  SIZE << name << COMMA << 8 << endl << 
  name << ":" << endl << 
  BOOLTAG << 0 << endl;
}

Symbol name_global[100]; // load global vars

void code_global_data(Decls decls, ostream &str)   
{ 
  int flag=0;
  int j=0;
   for (int i = decls->first(); decls->more(i); i = decls->next(i)) {
        if(!decls->nth(i)->isCallDecl()){
            if(flag==0){str<<DATA<<endl;flag=1;} // if global_data exist then cout .data
            if(decls->nth(i)->getType()==Int) emit_global_int(decls->nth(i)->getName(),str);
            if(decls->nth(i)->getType()==Float) emit_global_float(decls->nth(i)->getName(),str);
            if(decls->nth(i)->getType()==Bool) emit_global_bool(decls->nth(i)->getName(),str);
            name_global[j++]=decls->nth(i)->getName();
        }
    }
  str<<SECTION<<RODATA<<endl;
  stringtable.code_string_table(str);
  str<<TEXT<<endl;
}

void code_calls(Decls decls, ostream &str) {
  for (int i = decls->first(); decls->more(i); i = decls->next(i)) {
    if(decls->nth(i)->isCallDecl()){
        Symbol name = decls->nth(i)->getName();
        Symbol type = decls->nth(i)->getType();
        str << GLOBAL << name << endl << SYMBOL_TYPE << name << COMMA << FUNCTION << endl<< name << ":" << endl;
        decls->nth(i)->code(str); 
        CallType[name]=type;
    }
  }
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************



//********************************************************
//
// Cgen helper helps to initialize and call code() function.
// You can do any initializing operations here
//
//********************************************************

void cgen_helper(Decls decls, ostream& s)
{

  code(decls, s);
}


void code(Decls decls, ostream& s)
{
  if (cgen_debug) cout << "Coding global data" << endl;
  code_global_data(decls, s);

  if (cgen_debug) cout << "Coding calls" << endl;
  code_calls(decls, s);
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `seal-decl.h', `seal-expr.h' and `seal-stmt.h'
//   Sample code for constant integers, strings, and booleans are provided.
//   
//*****************************************************************

struct circle
{
  int begin=-1;
  int end=-1;
};
bool is_first_func=1;
stack<circle> circleStack;
string name_array[10000] = {};
stack<string> varStack;
int length=0;
int pos_num=-1;


void CallDecl_class::code(ostream &s) {
  int  f_num=0;
  if(is_first_func)
    length=7;
  else
    length=0;
  is_first_func=0;
  EmitPushes(s);

  for (int i = paras->first(); paras->more(i); i = paras->next(i)) {
    emit_sub("$8",RSP,s);
    if(paras->nth(i)->getType()!=Float)
      emit_rmmov(CALL_REGS[i- f_num],-8*(length+1),RBP,s);
    else
      emit_rmmov(CALL_XMM[f_num++],-8*(length+1),RBP,s);
    
    name_array[length]=paras->nth(i)->getName()->get_string();
    length++;
  }

  body->code(s);
  s<<SIZE<<name<<COMMA<<".-"<<name<<endl;
}

void StmtBlock_class::code(ostream &s){
  for (int i = vars->first(); vars->more(i); i = vars->next(i)) {
    emit_sub("$8",RSP,s);
    name_array[length++]=vars->nth(i)->getName()->get_string();
  }

  for (int i = stmts->first(); stmts->more(i); i = stmts->next(i)) {
    stmts->nth(i)->code(s);
  }
}

void IfStmt_class::code(ostream &s) {
  condition->code(s);
  int index=-1;
  for(int i=length-1;i>=0;i--){
      if (varStack.top()==name_array[i]){
        index=i;
        varStack.pop();
        break;
      }
  }//index<0时为全局变量
  if(index>=0) 
    emit_mrmov(RBP,-8*index-8,RAX,s);
  else 
    s<<MOV<<varStack.top()<<"("<<RIP<<")"<<COMMA<<RAX<<endl;

  int tmp_pos=pos_num;
  pos_num+=2;
  emit_test(RAX,RAX,s);
  emit_jz((".POS"+to_string(tmp_pos+1)).c_str(),s);
  thenexpr->code(s);

  emit_jmp((".POS"+to_string(tmp_pos+2)).c_str(),s);
  emit_position((".POS"+to_string(tmp_pos+1)).c_str(),s);
  elseexpr->code(s);

  emit_position((".POS"+to_string(tmp_pos+2)).c_str(),s);
}

void WhileStmt_class::code(ostream &s) {
  circle a;
  a.begin=pos_num+1;
  a.end=pos_num+2;
  circleStack.push(a);

  int tmp_pos=pos_num;
  pos_num+=2;
  emit_position((".POS"+to_string(tmp_pos+1)).c_str(),s);
  condition->code(s);
  int index=-1;
  for(int i=length-1;i>=0;i--){
    if (varStack.top()==name_array[i]){
      index=i;
      varStack.pop();
      break;
    }
  }//index<0时为全局变量
  if(index>=0) 
    emit_mrmov(RBP,-8*index-8,RAX,s);
  else 
    s<<MOV<<varStack.top()<<"("<<RIP<<")"<<COMMA<<RAX<<endl;

  emit_test(RAX,RAX,s);
  emit_jz((".POS"+to_string(tmp_pos+2)).c_str(),s);
  body->code(s);

  emit_jmp((".POS"+to_string(tmp_pos+1)).c_str(),s);
  emit_position((".POS"+to_string(tmp_pos+2)).c_str(),s);
  circleStack.pop();
}

void ForStmt_class::code(ostream &s) {
  circle a;
  a.begin=pos_num+1;
  a.end=pos_num+3;
  circleStack.push(a);

  int tmp_pos=pos_num;
  pos_num+=3;
  if(!initexpr->is_empty_Expr()) initexpr->code(s);
  emit_position((".POS"+to_string(tmp_pos+1)).c_str(),s);
  if(!condition->is_empty_Expr()) {
    condition->code(s);
    int index=-1;

    for(int i=length-1;i>=0;i--){
      if (varStack.top()==name_array[i]){
        index=i;
        varStack.pop();
        break;
      }
    }//index<0时为全局变量

    if(index>=0) emit_mrmov(RBP,-8*index-8,RAX,s);
    else {s<<MOV<<varStack.top()<<"("<<RIP<<")"<<COMMA<<RAX<<endl;}
    emit_test(RAX,RAX,s);
    emit_jz((".POS"+to_string(tmp_pos+3)).c_str(),s);
  }
  
  body->code(s);
  emit_position((".POS"+to_string(tmp_pos+2)).c_str(),s);
  if(!loopact->is_empty_Expr()){
    loopact->code(s);
  }
  emit_jmp((".POS"+to_string(tmp_pos+1)).c_str(),s);
  emit_position((".POS"+to_string(tmp_pos+3)).c_str(),s);
  circleStack.pop();
}

void ReturnStmt_class::code(ostream &s) {
  if(!value->is_empty_Expr()){
    value->code(s);
    int index=-1;
    for(int i=length-1;i>=0;i--){
      if (varStack.top()==name_array[i]){
        index=i;
        break;
      }
  }//index<0时为全局变量
  if(index>=0) {
    if(value->getType()!=Float) 
      s<<MOV<<-8*index-8<<"("<<RBP<<")"<<COMMA<<RAX<<endl;
    else 
      s<<MOVAPS<<-8*index-8<<"("<<RBP<<")"<<COMMA<<XMM0<<endl;
  }
    else {
      if(value->getType()!=Float) 
        s<<MOV<<varStack.top()<<"("<<RIP<<")"<<COMMA<<RAX<<endl;
      else 
        s<<MOVAPS<<varStack.top()<<"("<<RIP<<")"<<COMMA<<XMM0<<endl;
    }
  }
  EmitPops(s);
}

void ContinueStmt_class::code(ostream &s) {
  int begin=circleStack.top().begin;
  emit_jmp((".POS"+to_string(begin)).c_str(),s);
}

void BreakStmt_class::code(ostream &s) {
  int end=circleStack.top().end;
  emit_jmp((".POS"+to_string(end)).c_str(),s);
}

//参数调用时入栈参数~~~~~~~~~
int current_length = 0;
bool isGlobal = 0;
Symbol pname;

void Call_class::code(ostream &s) {
  int f_number=0;
  int nf_number = 0;
  queue<callTable> callQueue;

  for(int i=actuals->first();actuals->more(i); i = actuals->next(i)){
    actuals->nth(i)->code(s);
    if(isGlobal){
      callTable table;
      table.is_glb = 1;
      table.name = pname;
      table.is_flt = (actuals->nth(i)->getType()==Float);
      callQueue.push(table);
    }
    else{
      callTable table;
      table.cur_length = current_length;
      table.is_flt = (actuals->nth(i)->getType()==Float);
      callQueue.push(table);
    }
  }
  callTable ind;
  for(ind = callQueue.front();!callQueue.empty();callQueue.pop()){
    ind = callQueue.front();
    if (ind.is_glb)
    {
      if (ind.is_flt)
        s << MOVSD << ind.name << "(" << RIP << ")" << COMMA << CALL_XMM[f_number++] << endl;
      else
        s << MOV << ind.name << "(" << RIP << ")" << COMMA << CALL_REGS[nf_number++] << endl;
    }
    else
    {
      if(ind.is_flt)
      {
        emit_movsd(("-"+to_string(8 * ind.cur_length)+"("+RBP+")").c_str(),CALL_XMM[f_number++],s);
      }
      else
        emit_mrmov(RBP,-8 * ind.cur_length,CALL_REGS[nf_number++],s);
    }
  }
  
  //如果函数为printf
  if (name==print){
    string k=to_string(f_number);
    emit_sub("$8", RSP, s);
    emit_irmovl((char*)k.c_str(), EAX, s);
    emit_call("printf", s);
    length++;
  }
  else{
    emit_call(name->get_string(),s);
    if (CallType[name]!=Void){
      emit_sub("$8", RSP, s);
      name_array[length] = "Add" + to_string(length);
      varStack.push(name_array[length]);
      length++;
      if (CallType[name] == Float)
      {
        emit_rmmov(XMM0, -8 * length, RBP, s);
      }
      else
        emit_rmmov(RAX, -8 * length, RBP, s);
    }
    isGlobal = 0;
    current_length = length;    //不会存在使用void为参数的情况
  }
}


void Actual_class::code(ostream &s) {
  expr->code(s);
}

void Assign_class::code(ostream &s) {
  value->code(s);
  int l_index = -1, r_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (lvalue->get_string() == name_array[i])
    {
      l_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (varStack.top() == name_array[i])
    {
      r_index = i;
      break;
    }
  }

  if(r_index>=0)
    emit_mrmov(RBP, -8 * r_index - 8, RAX, s);
  else
  {
    s << MOV << varStack.top() << "(" << RIP << ")" << COMMA << RAX << endl;
  }
  if (l_index >= 0)
  {
    emit_rmmov(RAX, -8 * l_index - 8, RBP, s);
    isGlobal = 0;
    current_length = l_index + 1;
  }
  else
  {
    s << MOV << RAX << COMMA << lvalue << "(" << RIP << ")" << endl;
    isGlobal = 1;
    pname = lvalue;
  }
}

void Add_class::code(ostream &s) {
  
  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }

  emit_sub("$8", RSP, s);
  name_array[length]="Add"+to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RBX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RBX << endl;
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, R10, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << R10 << endl;
    emit_add(RBX, R10, s);
    emit_rmmov(R10, -8 * length, RBP, s);
  }//整数与整数相加
  else{
    if(e1->getType()==Float&&e2->getType()==Float){
      if (a1_index >= 0)
        s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM4 << endl;
      else
        s << MOVSD << a1 << "(" << RIP << ")" << COMMA << XMM4 << endl;
      if (a2_index >= 0)
        s << MOVSD << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM5 << endl;
      else
        s << MOVSD << a2 << "(" << RIP << ")" << COMMA << XMM5 << endl;
    }
    if(e1->getType()==Int&&e2->getType()==Float){
      if (a1_index >= 0)
        emit_mrmov(RBP, -8 * a1_index - 8, RBX, s);
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << RBX << endl;
      emit_int_to_float(RBX, XMM4, s);
      if (a2_index >= 0)
        s << MOVSD << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM5 << endl;
      else
        s << MOVSD << a2 << "(" << RIP << ")" << COMMA << XMM5 << endl;
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if (a1_index >= 0)
        s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM4 << endl;
      else
        s << MOVSD << a1 << "(" << RIP << ")" << COMMA << XMM4 << endl;
      if (a2_index >= 0)
        emit_mrmov(RBP, -8 * a2_index - 8, RBX, s);
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << RBX << endl;
      emit_int_to_float(RBX, XMM5, s);
    }

    emit_addsd(XMM4,XMM5,s);
    s<<MOVSD<<XMM5<<COMMA<<-8*length<<"("<<RBP<<")"<<endl;
  }
  isGlobal = 0;
  current_length = length;
}

void Minus_class::code(ostream &s) {
  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }

  emit_sub("$8", RSP, s); 
  name_array[length]="Add"+to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if(a1_index>=0) 
      emit_mrmov(RBP,-8*a1_index-8,RBX,s);
    else 
      s<<MOV<<a1<<"("<<RIP<<")"<<COMMA<<RBX<<endl;
    if(a2_index>=0) 
      emit_mrmov(RBP,-8*a2_index-8,R10,s);
    else 
      s<<MOV<<a2<<"("<<RIP<<")"<<COMMA<<R10<<endl;
    emit_sub(R10,RBX,s);
    emit_rmmov(RBX,-8*length,RBP,s);
  }//整数与整数相减
  else{
    if(e1->getType()==Float&&e2->getType()==Float){
      if(a1_index>=0) 
        s<<MOVSD<<-8*a1_index-8<<"("<<RBP<<")"<<COMMA<<XMM4<<endl;
      else 
        s<<MOVSD<<a1<<"("<<RIP<<")"<<COMMA<<XMM4<<endl;
      if(a2_index>=0) 
        s<<MOVSD<<-8*a2_index-8<<"("<<RBP<<")"<<COMMA<<XMM5<<endl;
      else 
        s<<MOVSD<<a2<<"("<<RIP<<")"<<COMMA<<XMM5<<endl;
    }
    if(e1->getType()==Int&&e2->getType()==Float){
      if(a1_index>=0) 
        emit_mrmov(RBP,-8*a1_index-8,RBX,s);
      else 
        s<<MOV<<a1<<"("<<RIP<<")"<<COMMA<<RBX<<endl;

      emit_int_to_float(RBX,XMM4,s);

      if(a2_index>=0) 
        s<<MOVSD<<-8*a2_index-8<<"("<<RBP<<")"<<COMMA<<XMM5<<endl;
      else 
        s<<MOVSD<<a2<<"("<<RIP<<")"<<COMMA<<XMM5<<endl;
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if(a1_index>=0) 
        s<<MOVSD<<-8*a1_index-8<<"("<<RBP<<")"<<COMMA<<XMM4<<endl;
      else 
        s<<MOVSD<<a1<<"("<<RIP<<")"<<COMMA<<XMM4<<endl;
      if(a2_index>=0) 
        emit_mrmov(RBP,-8*a2_index-8,RBX,s);
      else 
        s<<MOV<<a2<<"("<<RIP<<")"<<COMMA<<RBX<<endl;
      emit_int_to_float(RBX,XMM5,s);
    }
    emit_subsd(XMM5,XMM4,s);
    s<<MOVSD<<XMM4<<COMMA<<-8*length<<"("<<RBP<<")"<<endl;
  }
  isGlobal = 0; 
  current_length = length;
}

void Multi_class::code(ostream &s) {

  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }

  emit_sub("$8", RSP, s); 
  name_array[length]="Add"+to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RBX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RBX << endl;
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, R10, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << R10 << endl;
    emit_mul(R10, RBX, s);
    emit_rmmov(RBX, -8 * length, RBP, s);
  }//整数与整数相乘
  else{
    if(e1->getType()==Float&&e2->getType()==Float){
      if (a1_index >= 0)
        s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM4 << endl;
      else
        s << MOVSD << a1 << "(" << RIP << ")" << COMMA << XMM4 << endl;
      if (a2_index >= 0)
        s << MOVSD << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM5 << endl;
      else
        s << MOVSD << a2 << "(" << RIP << ")" << COMMA << XMM5 << endl;
    }
    if(e1->getType()==Int&&e2->getType()==Float){
      if (a1_index >= 0)
        emit_mrmov(RBP, -8 * a1_index - 8, RBX, s);
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << RBX << endl;
      emit_int_to_float(RBX, XMM4, s);
      if (a2_index >= 0)
        s << MOVSD << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM5 << endl;
      else
        s << MOVSD << a2 << "(" << RIP << ")" << COMMA << XMM5 << endl;
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if (a1_index >= 0)
        s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM4 << endl;
      else
        s << MOVSD << a2 << "(" << RIP << ")" << COMMA << XMM4 << endl;
      if (a2_index >= 0)
        emit_mrmov(RBP, -8 * a2_index - 8, RBX, s);
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << RBX << endl;
      emit_int_to_float(RBX, XMM5, s);
    }
    emit_mulsd(XMM5,XMM4,s);
    s<<MOVSD<<XMM4<<COMMA<<-8*length<<"("<<RBP<<")"<<endl;
  }
  isGlobal = 0; current_length = length;
}

void Divide_class::code(ostream &s) {
   
  e1->code(s);
  string a1=varStack.top();
  varStack.pop();
  e2->code(s);
  string a2=varStack.top();
  varStack.pop();
  int a1_index=-1,a2_index=-1;
  for(int i=length-1;i>=0;i--){
    if (a1==name_array[i]){
      a1_index=i;
      break;
    }
  }//index<0时为全局变量
  for(int i=length-1;i>=0;i--){
    if (a2==name_array[i]){
      a2_index=i;
      break;
    }
  }

  emit_sub("$8", RSP, s); 
  name_array[length]="Add"+to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
    emit_cqto(s);
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, RBX, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << RBX << endl;
    emit_div(RBX, s);
    emit_rmmov(RAX, -8 * length, RBP, s);
  }//整数与整数相除
  else{
    if(e1->getType()==Float&&e2->getType()==Float){
      if (a1_index >= 0)
        s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM4 << endl;
      else
        s << MOVSD << a1 << "(" << RIP << ")" << COMMA << XMM4 << endl;
      if (a2_index >= 0)
        s << MOVSD << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM5 << endl;
      else
        s << MOVSD << a2 << "(" << RIP << ")" << COMMA << XMM5 << endl;
    }
    if(e1->getType()==Int&&e2->getType()==Float){
      if (a1_index >= 0)
        emit_mrmov(RBP, -8 * a1_index - 8, RBX, s);
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << RBX << endl;
      emit_int_to_float(RBX, XMM4, s);
      if (a2_index >= 0)
        s << MOVSD << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM5 << endl;
      else
        s << MOVSD << a2 << "(" << RIP << ")" << COMMA << XMM5 << endl;
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if (a1_index >= 0)
        s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM4 << endl;
      else
        s << MOVSD << a1 << "(" << RIP << ")" << COMMA << XMM4 << endl;
      if (a2_index >= 0)
        emit_mrmov(RBP, -8 * a2_index - 8, RBX, s);
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << RBX << endl;
      emit_int_to_float(RBX, XMM5, s);
    }
    emit_divsd(XMM5,XMM4,s);
    s<<MOVSD<<XMM4<<COMMA<<-8*length<<"("<<RBP<<")"<<endl;
  }
  isGlobal = 0;
  current_length = length;
}

void Mod_class::code(ostream &s) {

  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }

  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if(a1_index>=0)
      emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
    emit_cqto(s);
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, RBX, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << RBX << endl;
    emit_div(RBX, s);
    emit_rmmov(RDX, -8 * length, RBP, s);
  }
  isGlobal = 0;
  current_length = length;
}

void Neg_class::code(ostream &s) {
    
  e1->code(s);
  string a1=varStack.top();
  varStack.pop();
  int a1_index=-1,a2_index=-1;
  for(int i=length-1;i>=0;i--){
    if (a1==name_array[i]){
      a1_index=i;
      break;
    }
  }//index<0时为全局变量
  emit_sub("$8", RSP, s); 
  name_array[length]="Add"+to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
    emit_neg(RAX, s);
    emit_rmmov(RAX, -8 * length, RBP, s);
  }
  if(e1->getType()==Float){
    emit_irmov("0x8000000000000000", RAX, s);
    if (a1_index >= 0)
      s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << RDX << endl;
    else
      s << MOVSD << a1 << "(" << RIP << ")" << COMMA << RDX << endl;
    emit_xor(RAX, RDX, s);
    emit_rmmov(RDX, -8 * length, RBP, s);
  }
  isGlobal = 0;
  current_length = length;
}

void Lt_class::code(ostream &s) {

  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }
  int tmp_pos = pos_num;
  pos_num += 2;
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
    emit_cmp(RDX, RAX, s);
    emit_jl((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  }//整数与整数
  else{
    if(e1->getType()==Float&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        s << MOV << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }

    if(e1->getType()==Int&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM1, s);
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if (a2_index >= 0)
        emit_mrmov(RBP, -8 * a2_index - 8, RAX, s);
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM0, s);
      if (a1_index >= 0)
        s << MOV << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }
    emit_ucompisd(XMM0,XMM1,s);
    emit_jb((".POS"+to_string(tmp_pos+1)).c_str(),s);
  }
  emit_irmov("0", RAX, s);
  emit_jmp((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_position((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  emit_irmov("1", RAX, s);
  emit_position((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_rmmov(RAX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void Le_class::code(ostream &s) {

  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }

  int tmp_pos = pos_num;
  pos_num += 2;
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
    emit_cmp(RDX, RAX, s);
    emit_jle((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  }//整数与整数
  else{
    if (e1->getType() == Float && e2->getType() == Float)
    {
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        s << MOV << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }

    if(e1->getType()==Int&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM1, s);
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if (a2_index >= 0)
        emit_mrmov(RBP, -8 * a2_index - 8, RAX, s);
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM0, s);
      if (a1_index >= 0)
        s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }
    emit_ucompisd(XMM0,XMM1,s);
    emit_jbe((".POS"+to_string(tmp_pos+1)).c_str(),s);
  }
  emit_irmov("0", RAX, s);
  emit_jmp((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_position((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  emit_irmov("1", RAX, s);
  emit_position((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_rmmov(RAX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void Equ_class::code(ostream &s) {

  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }
  int tmp_pos = pos_num;
  pos_num += 2;
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
    emit_cmp(RDX, RAX, s);
    emit_je((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  }//整数与整数
  else{
    if(e1->getType()==Float&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }

    if(e1->getType()==Int&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM1, s);
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if (a2_index >= 0)
        emit_mrmov(RBP, -8 * a2_index - 8, RAX, s);
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM0, s);
      if (a1_index >= 0)
        s << MOV << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }
    emit_ucompisd(XMM0,XMM1,s);
    emit_je((".POS"+to_string(tmp_pos+1)).c_str(),s);
  }
  emit_irmov("0",RAX,s);
  emit_jmp((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_position((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  emit_irmov("1", RAX, s);
  emit_position((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_rmmov(RAX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void Neq_class::code(ostream &s) {
  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }
  int tmp_pos = pos_num;
  pos_num += 2;
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
    emit_cmp(RDX, RAX, s);
    emit_jne((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  }//整数与整数
  else{
    if (e1->getType() == Float && e2->getType() == Float)
    {
      if (a2_index >= 0)
        s << MOVSD << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        s << MOVSD << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }

    if(e1->getType()==Int&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM1, s);
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if (a2_index >= 0)
        emit_mrmov(RBP, -8 * a2_index - 8, RAX, s);
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM0, s);
      if (a1_index >= 0)
        s << MOV << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }
    emit_ucompisd(XMM0, XMM1, s);
    emit_jne((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  }
  emit_irmov("0", RAX, s);
  emit_jmp((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_position((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  emit_irmov("1", RAX, s);
  emit_position((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_rmmov(RAX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void Ge_class::code(ostream &s) {
    
  e1->code(s);
  string a1=varStack.top();
  varStack.pop();
  e2->code(s);
  string a2=varStack.top();
  varStack.pop();
  int a1_index=-1,a2_index=-1;
  for(int i=length-1;i>=0;i--){
    if (a1==name_array[i]){
      a1_index=i;
      break;
    }
  }//index<0时为全局变量
  for(int i=length-1;i>=0;i--){
    if (a2==name_array[i]){
      a2_index=i;
      break;
    }
  }
  int tmp_pos=pos_num;
  pos_num+=2;
  emit_sub("$8", RSP, s); 
  name_array[length]="Add"+to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
    emit_cmp(RDX, RAX, s);
    emit_jge((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  }//整数与整数
  else{
    if(e1->getType()==Float&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        s << MOV << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }

    if(e1->getType()==Int&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM1, s);
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if (a2_index >= 0)
        emit_mrmov(RBP, -8 * a2_index - 8, RAX, s);
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM0, s);
      if (a1_index >= 0)
        s << MOV << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }
    emit_ucompisd(XMM0,XMM1,s);
    emit_jae((".POS"+to_string(tmp_pos+1)).c_str(),s);
  }
  emit_irmov("0", RAX, s);
  emit_jmp((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_position((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  emit_irmov("1", RAX, s);
  emit_position((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_rmmov(RAX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void Gt_class::code(ostream &s) {

  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }
  int tmp_pos = pos_num;
  pos_num+=2;
  emit_sub("$8", RSP, s); 
  name_array[length]="Add"+to_string(length);
  varStack.push(name_array[length]);
  length++;
  if(e1->getType()==Int&&e2->getType()==Int){
    if (a1_index >= 0)
      emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
    else
      s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
    if (a2_index >= 0)
      emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
    else
      s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
    emit_cmp(RDX, RAX, s);
    emit_jg((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  }//整数与整数
  else{
    if(e1->getType()==Float&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        s << MOV << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }

    if(e1->getType()==Int&&e2->getType()==Float){
      if (a2_index >= 0)
        s << MOV << -8 * a2_index - 8 << "(" << RBP << ")" << COMMA << XMM0 << endl;
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << XMM0 << endl;
      if (a1_index >= 0)
        emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM1, s);
    }
    if(e1->getType()==Float&&e2->getType()==Int){
      if (a2_index >= 0)
        emit_mrmov(RBP, -8 * a2_index - 8, RAX, s);
      else
        s << MOV << a2 << "(" << RIP << ")" << COMMA << RAX << endl;
      emit_int_to_float(RAX, XMM0, s);
      if (a1_index >= 0)
        s << MOV << -8 * a1_index - 8 << "(" << RBP << ")" << COMMA << XMM1 << endl;
      else
        s << MOV << a1 << "(" << RIP << ")" << COMMA << XMM1 << endl;
    }
    emit_ucompisd(XMM0,XMM1,s);
    emit_ja((".POS"+to_string(tmp_pos+1)).c_str(),s);
  }

  emit_irmov("0", RAX, s);
  emit_jmp((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_position((".POS" + to_string(tmp_pos + 1)).c_str(), s);
  emit_irmov("1", RAX, s);
  emit_position((".POS" + to_string(tmp_pos + 2)).c_str(), s);
  emit_rmmov(RAX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void And_class::code(ostream &s) {
    
  e1->code(s);
  string a1=varStack.top();
  varStack.pop();
  e2->code(s);
  string a2=varStack.top();
  varStack.pop();
  int a1_index=-1,a2_index=-1;
  for(int i=length-1;i>=0;i--){
    if (a1==name_array[i]){
      a1_index=i;
      break;
    }
  }//index<0时为全局变量
  for(int i=length-1;i>=0;i--){
    if (a2==name_array[i]){
      a2_index=i;
      break;
    }
  }
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if (a1_index >= 0)
    emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
  else
    s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
  if (a2_index >= 0)
    emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
  else
    s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
  emit_and(RAX, RDX, s);
  emit_rmmov(RDX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void Or_class::code(ostream &s) {
    
  e1->code(s);
  string a1=varStack.top();
  varStack.pop();
  e2->code(s);
  string a2=varStack.top();
  varStack.pop();
  int a1_index=-1,a2_index=-1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }

  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if (a1_index >= 0)
    emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
  else
    s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
  if (a2_index >= 0)
    emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
  else
    s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
  emit_or(RAX, RDX, s);
  emit_rmmov(RDX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void Xor_class::code(ostream &s) {

  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if (a1_index >= 0)
    emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
  else
  {
    s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
  }
  if (a2_index >= 0)
    emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
  else
  {
    s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
  }
  emit_xor(RAX, RDX, s);
  emit_rmmov(RDX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void Not_class::code(ostream &s) {
  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  int a1_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if (a1_index >= 0)
    emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
  else
    s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
  emit_irmov("0x0000000000000001", RDX, s);
  emit_xor(RDX, RAX, s);
  emit_rmmov(RDX, -8 * length, RBP, s);

  isGlobal = 0; 
  current_length = length;
}

void Bitnot_class::code(ostream &s) {

  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  int a1_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if (a1_index >= 0)
    emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
  else
    s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
  emit_not(RAX, s);
  emit_rmmov(RAX, -8 * length, RBP, s);

  isGlobal = 0; 
  current_length = length;
}

void Bitand_class::code(ostream &s) {
    
  e1->code(s);
  string a1=varStack.top();
  varStack.pop();
  e2->code(s);
  string a2=varStack.top();
  varStack.pop();
  int a1_index=-1,a2_index=-1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if (a1_index >= 0)
    emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
  else
  {
    s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
  }
  if (a2_index >= 0)
    emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
  else
  {
    s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
  }
  emit_and(RAX,RDX,s);
  emit_rmmov(RDX,-8*length,RBP,s);
   
  isGlobal = 0; 
  current_length = length;
}

void Bitor_class::code(ostream &s) {

  e1->code(s);
  string a1 = varStack.top();
  varStack.pop();
  e2->code(s);
  string a2 = varStack.top();
  varStack.pop();
  int a1_index = -1, a2_index = -1;
  for (int i = length - 1; i >= 0; i--)
  {
    if (a1 == name_array[i])
    {
      a1_index = i;
      break;
    }
  } //index<0时为全局变量
  for (int i = length - 1; i >= 0; i--)
  {
    if (a2 == name_array[i])
    {
      a2_index = i;
      break;
    }
  }
  emit_sub("$8", RSP, s);
  name_array[length] = "Add" + to_string(length);
  varStack.push(name_array[length]);
  length++;
  if (a1_index >= 0)
    emit_mrmov(RBP, -8 * a1_index - 8, RAX, s);
  else
  {
    s << MOV << a1 << "(" << RIP << ")" << COMMA << RAX << endl;
  }
  if (a2_index >= 0)
    emit_mrmov(RBP, -8 * a2_index - 8, RDX, s);
  else
  {
    s << MOV << a2 << "(" << RIP << ")" << COMMA << RDX << endl;
  }
  emit_xor(RAX,RDX,s);
  emit_rmmov(RDX,-8*length,RBP,s);
   
  isGlobal = 0; 
  current_length = length;
}

void Const_int_class::code(ostream &s) {
  name_array[length] = value->get_string();
  length++;
  emit_sub("$8", RSP, s);
  emit_irmov(value->get_string(), RAX, s);
  emit_rmmov(RAX, -8 * length, RBP, s);
  varStack.push(value->get_string());
  isGlobal = 0;
  current_length = length;
}

void Const_string_class::code(ostream &s) {
  name_array[length] = value->get_string();
  varStack.push(name_array[length]);
  length++;
  emit_sub("$8", RSP, s);
  int count = -1;
  for (int i = 0; i < 1000; i++)
  {
    if (value->equal_index(i))
    {
      count = i;
      break;
    }
  }
  string m = to_string(count);
  m.insert(0, "$.LC");
  emit_mov((char *)m.c_str(), RAX, s);
  emit_rmmov(RAX, -8 * length, RBP, s);
  isGlobal = 0;
  current_length = length;
}

void Const_float_class::code(ostream &s) {
  name_array[length]=value->get_string();
  length++;
  string k=value->get_string();
  int len=value->get_len();
  const char *p = k.c_str();
  int f[8];
  int intPart=0;
  int base=10;
  double result=0;
  bool flag=1;
  for(int i=0;i<len;i++){
    if(p[i]=='.'){flag=0;continue;}
    if(flag){
      intPart=intPart*base+(p[i]-'0');
    }
    else{
      result=result+(double)(p[i]-'0')/base;
      base*=10;
    }
  }
  result=result+intPart;
  unsigned char *hexl=(unsigned char* )&result;
  emit_sub("$8",RSP,s);

  s << MOV << "$0x" ;
  for(int i=7;i>=0;i--){
    f[i]=hexl[i];

    if (f[i] < 16)
      s<<'0';

    s<<hex<<f[i];
  }
  s << dec;
  s << COMMA << RAX  << endl;
  emit_rmmov(RAX,-8*length,RBP,s);
  varStack.push(value->get_string());
  isGlobal = 0; 
  current_length = length;
}

void Const_bool_class::code(ostream &s) {
  name_array[length]="Bool"+to_string(length);
  length++;
  emit_sub("$8",RSP,s);

  char *boolean;
  if(value)
    boolean="1";
  else
    boolean="0";
  emit_irmov(boolean,RAX,s);
  emit_rmmov(RAX,-8*length,RBP,s);
  varStack.push("Bool"+to_string(length));
  isGlobal = 0; 
  current_length = length;
}

void Object_class::code(ostream &s) {
  varStack.push(var->get_string());
  int index=-1;
  for(int m=length-1;m>=0;m--){
    if (var->get_string()==name_array[m]){
      index=m;
      break;
    }
  }//index<0时为全局变量
  if(index>=0) {
    isGlobal = 0; 
    current_length = index + 1;
  }
  else {
    isGlobal = 1; 
    pname = var;
  }
}

void No_expr_class::code(ostream &s) {

}