#include "rwmake.ch"
#include "topconn.ch"
#include "Protheus.ch"

// цдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцддцдцдцдцдцдцдцдцдддддддддддд
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor      = Felipe Alexandre Soares                                                                                                         |
// | Dt.Inicial = 26/02/2008                                                                                                                      |
// | Descricao  = Contabilizar o rateio do custo do MC do Veiling                                                                                 |
// | Uso        = PFT/BUL                                                                                                                         |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// |                                    A L T E R A C O E S    E F E T U A D A S                                                                  |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor                  | Chamado  | Data       | DescriГЦo                                                                                   |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// | Felipe A Soares        |          | 03/04/2009 | IncluМda rotina para nЦo deixar alterar quando o pagamento jА foi contabilizado.            |
// |                        |          |            | IncluМdo campo data de lanГamento contabilizado. (Z0I_DTLANC)                               |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// | JoЦo Anaia Ternero     | 00001992 | 19/07/2010 | Alterada a funГЦo KsTela3 para Modelo3.                                                     |
// |                        |          |            | Incluidas funГУes de calculo do total no rodapИ para os campos Valor, Tx Extra e Total Geral|
// |                        |          |            | Incluida validaГЦo no TdOk para nЦo confirmar a tela se todos os itens estiverem deletados. |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// | David Fernando Pulz    |          | 31/12/2011 | Incluida verificaГЦo das tabelas exclusivas ou compartilhadas e comparaГЦo da Filial.       |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// | David Fernando Pulz    | P12      | 22/08/2017 | Alterado RegToMemory(Alias, .T., .T. )                                                      |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// цдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцддцдцдцдцдцдцдцдцдддддддддддд
User Function KSCADZ0I()
Private cCadastro := "Cadastro de Pagamentos do Material Circulante"
Private aRotina   := {;
{"Pesquisar"	,"AxPesqui"	,0,1},;
{"Visualizar"	,"U_KSCZ0I"	,0,2},;
{"Incluir"		,"U_KSCZ0I"	,0,3},;
{"Alterar"		,"U_KSCZ0I"	,0,4},;
{"Excluir"		,"U_KSCZ0I"	,0,5};
}

Z0I->(dbSetOrder(1))
mBrowse(,,,,"Z0I")
Z0I->(dBclearFilter())

Return nil

//---------------------------------------------------------------------------
//VisualisaГЦo,InclusЦo,alteraГЦo e ExclusЦo.
//---------------------------------------------------------------------------
User Function KSCZ0I(cAlias,nReg,nOpc)
//-VariАvel de controle de campos a nЦo exibir no cabeГalho, NцO ALTERа-LA.
Local aCpoZ0I := {"NOUSER"}

//-Incluir nesta variАvel os campos a nЦo exibir no cabeГalho
Local aNoInZ0I := {"Z0I_ITEM","Z0I_DATADE","Z0I_DATAAT","Z0I_CODMC","Z0I_DESC","Z0I_VALOR","Z0I_EXTRA"}

//-variАvel que serА utilizada para informar o tipo de operaГЦo de gravaГЦo utlizadas pelas funГУes de gravaГЦo.
Local cOper

//-Matriz que serА utilizada pela funГЦo de gravaГЦo dos detalhes de cada tabela.
Local aChZ0I := {}

//-VariАvels que conterЦo a query de retorno de informaГУes para os detalhes.
Local cQryZ0I := ""

//-variАvel que indica a atualizaГЦo e redesenho da(s) tela(s) de detalhes
Local lPinta := .T.

//-variАvel que receberА o objeto da tela principal
Local oDlg

//-variАvel para guardar a ultima posiГЦo do registro da tabela principal
Local nRegZ0I := 0

//-variАveis que definem o mАximo de itens para cada tabela de detalhes
Local nItZ0I  := 999

//---------------------------------------------------------------------
// VariАveis para verificaГЦo de tabelas exclusivas ou compartilhadas
//---------------------------------------------------------------------
Local cFilZ0V := ''
Local cFilZ0I := ''

SX2->(DbSetOrder(1))
cFilZ0V := alltrim(Posicione("SX2",1,"Z0V","X2_MODO"))
cFilZ0I := alltrim(Posicione("SX2",1,"Z0I","X2_MODO"))

If !inclui
	dbSelectArea(cAlias)
	If !SoftLock(cAlias)
		return .F.
	EndIf
EndIf

//-VariАvel que receberА o objeto pasta de cabeГalho
Private oPastaCb

//-VariАvel que receberА o objeto pasta de detalhes
Private oPastaDt

//-variАveis que receberА o objeto da tela de gets do cabeГalho
Private oEnchZ0I

//-variАveis para receber o objeto das telas detalhes
Private oGDetZ0I

//-Matriz para adiГЦo de botУes na barras de ferramentas
Private aButtons := {}

//-Matriz com titulos das abas do cabeГalho
Private aTitCab := {"&ContabilizaГЦo do Rateio de Mat. Circulante"}

//-Matriz com titulos das abas dos detalhes
Private aTitDet := {"&LanГamentos"}

//-variАvel que receberА os objetos na tela princial, gets do cabeГalho, detalhes, etc...
Private aObjects := {}

//-variАveis para dimensionamento das telas
Private aInfo    := {}
Private aPosObj  := {}
Private aSize    := MsAdvSize()

//-Matriz principal que recebe informaГУes especificas de posicionamento de gets na tela de cabeГalho
Private aTela   := {}

//-uma copia de aTela para cada tabela utilizada no cabeГalho
Private aTela1  := {}

//-Matriz principal que recebe informaГУes especificas dos campos utilizados na tela de cabeГalho
Private aGets   := {}

//-variАvel que receberЦo as informaГУes das colunas dos detalhes
Private aHeader  := {}
Private aHDetZ0I := {}

Private oSayZ0I01
Private oSayZ0I02
Private oSayZ0I03

//-variАvel que receberЦo os valores dos detalhes
Private aCols 	  := {}
Private aCDetZ0I := {}

//-variАvel de controle para definir o modo de ediГЦo das telas
Private _nOpc		:= iif(INCLUI .OR. ALTERA,GD_UPDATE+GD_DELETE+GD_INSERT,0)
Private _nOpcEnch	:= iif(INCLUI .OR. ALTERA,4,5)

//-variАvel que determina a opГЦo de confirmaГЦo da tela de lanГamentos, 1 =Ok 2=Cancela.
Private nOpcx := 0

//-adicionando todos os objetos na tela principal "oDlg"
aAdd( aObjects, { 100, 080, .T., .F. } ) //-tela de cabeГalho
aAdd( aObjects, { 100, 170, .T., .F. } ) //-tela de detalhes
AAdd( aObjects, { 100, 015, .T., .T. } ) //-total no rodapИ
aInfo   := { aSize[ 1 ], aSize[ 2 ], aSize[ 3 ], aSize[ 4 ], 3, 3 }
aPosObj := MsObjSize( aInfo, aObjects )

//-PosiГУes da(s) tela(s) de cabeГalho dentro da pasta de cabeГalho
nPGet1 := 2 //Pos x1
nPGet2 := 2 //Pos y1
nPGet3 := aPosObj[1,3]-aPosObj[1,1]-15 //-pos x2
nPGet4 := aPosObj[1,4]-aPosObj[1,2]-4 //-pos y2

//-PosiГУes da(s) tela(s) de detalhes dentro da pasta de detalhes
nGCols1:= 2 //Pos x1
nGCols2:= 2 //Pos y1
nGCols3:= aPosObj[2,3]-aPosObj[2,1]-20 //-pos x2
nGCols4:= aPosObj[2,4]-aPosObj[2,2]-4  //-pos y2

//-Carrega o(s) cabeГalho(s)
If Inclui
	RegToMemory("Z0I", .T., .T. )
	nRegZ0I := 0
Else
	nRegZ0I := Z0I->(Recno())
	RegToMemory("Z0I", .F., .T. )
EndIf

//-determina os campos a exibir no cabeГalho do alias nela informado.
aEval(ApBuildHeader("Z0I",aNoInZ0I), {|x| Aadd(aCpoZ0I,x[2])})

//-carrega os aHeaders de cada tabela de detalhe
aHDetZ0I := u_KsMkHead("Z0I",{"Z0I_CODIGO","Z0I_DTPGTO","Z0I_SITIO","Z0I_USER","Z0I_DTLANC","Z0I_UNID"})

cQryZ0I += " SELECT Z0I_CODIGO, Z0I_DTPGTO, Z0I_SITIO, Z0I_ITEM, Z0I_DATADE, Z0I_DATAAT, Z0I_CODMC, ZV_DESCRIC AS Z0I_DESC "
cQryZ0I += " , Z0I_VALOR, Z0I_EXTRA, Z0I_DTLANC, Z0I_UNID, Z0I_USER "
cQryZ0I += " FROM " + RetSqlName("Z0I")+ " AS Z0I "
cQryZ0I += " INNER JOIN " + RetSqlName("SZV") + " AS SZV "
cQryZ0I += " ON ZV_CODIGO = Z0I_CODMC AND SZV.D_E_L_E_T_ = '' "

If cFilZ0V = cFilZ0I  // compara filial - 31/12/2011 //
	cQuery += " AND Z0V.Z0V_FILIAL = Z0I.Z0I_FILIAL "
EndIf

cQryZ0I += " WHERE Z0I.D_E_L_E_T_ = '' AND Z0I_FILIAL = " +XFILIAL('Z0I')+ " AND Z0I_CODIGO = '"+Z0I->Z0I_CODIGO+"' "
cQryZ0I += " ORDER BY Z0I_CODIGO "

//-carrega os detalhes "aCols" de cada tabela.
aCDetZ0I := u_KsLAcols(nOpc,aHDetZ0I,"Z0I_ITEM",cQryZ0I)

// FunГУes de validaГЦo do cadastro
Private cLnOkZ0I	:= "U_KsLinZ0I()"
Private cTdOkZ0I	:= "U_KsTokZ0I()"
Private cFeOkZ0I	:= "U_KsFieZ0I()"
Private cDelOkZ0I	:= "U_KsDelZ0I()"

// Variaveis de posicionamento no aCols
//MapCampo()

DEFINE MSDIALOG oDlg TITLE cCadastro From aSize[7],00 To aSize[6],aSize[5] OF oMainWnd PIXEL

//-monta a pasta do cabeГalho
oPastaCb := TFolder():New(aPosObj[1,1],aPosObj[1,2],aTitCab,{'',''},oDlg,,,,.T.,.F.,;
aPosObj[1,4]-aPosObj[1,2],aPosObj[1,3]-aPosObj[1,1])

//-monta a pasta dos detalhes
oPastaDt := TFolder():New(aPosObj[2,1],aPosObj[2,2],aTitDet,{'',''},oDlg,,,,.T.,.F.,;
aPosObj[2,4]-aPosObj[2,2],aPosObj[2,3]-aPosObj[2,1])

//-monta uma tela de gets para o Plantio
oEnchZ0I := MsMGet():New("Z0I" ,nRegZ0I, _nOpcEnch ,,,,aCpoZ0I, {nPGet1,nPGet2,nPGet3,nPGet4},,,,,,oPastaCb:aDialogs[1],,.T.,,"aTela1" )
aGets1 := AClone( aGets )
aTela1 := AClone( aTela )
aTela  := {}
aGets  := {}

//-monta a tela de detalhes para o Manejo de plantio
oGDetZ0I := MsNewGetDados():New(nGCols1,nGCols2,nGCols3,nGCols4,_nOpc,cLnOkZ0I,cTdOkZ0I,"+Z0I_ITEM",,,nItZ0I,/*cFeOkZ0I*/,,cDelOKZ0I,oPastaDt:aDialogs[1],@aHDetZ0I,@aCDetZ0I)
oGDetZ0I :oBrowse:lDisablePaint := lPinta

oPanel  	:= TPanel():New(aPosObj[3,1],aPosObj[3,2],"",oDlg,,,,,CLR_LIGHTGRAY,aPosObj[3,4],15,.F.,.F.)
oFont   	:= TFont():New("Courier",08,15,.T.,.F.,5,.T.,5,.T.,.F.)
oSayToG  := TSay():New( 05,2, {|| space(85) + 'Total Geral : '},oPanel,,oFont,,,,.T., CLR_BLUE,CLR_WHITE,aPosObj[3,4]-10,10)
oSayTxE  := TSay():New( 05,2, {|| space(40) + 'Total Taxa Extra : '},oPanel,,oFont,,,,.T., CLR_BLUE,CLR_WHITE,aPosObj[3,4]-10,10)
oSayVlT  := TSay():New( 05,2, {|| 'Valor Total : '},oPanel,,oFont,,,,.T., CLR_BLUE,CLR_WHITE,aPosObj[3,4]-10,10)
oSayZ0I03:= TSay():New( 05,2, {|| CalcTotal()[3]},oPanel,,oFont,,,,.T., CLR_BLUE,CLR_WHITE,aPosObj[3,4]-10,10)
oSayZ0I02:= TSay():New( 05,2, {|| CalcTotal()[2]},oPanel,,oFont,,,,.T., CLR_BLUE,CLR_WHITE,aPosObj[3,4]-10,10)
oSayZ0I01:= TSay():New( 05,2, {|| CalcTotal()[1]},oPanel,,oFont,,,,.T., CLR_BLUE,CLR_WHITE,aPosObj[3,4]-10,10)

ACTIVATE MSDIALOG oDlg ON INIT (fAtuBrowse(),;
EnchoiceBar(oDlg,{|| ;
iif(Obrigatorio(aGets1,aTela1);
.and. oGDetZ0I:TudoOK(),;
(nOpcx:=1,oDlg:End()),nOpcx:= 0)},;
{|| oDlg:End()},,abuttons))

//- Define o tipo de operaГЦo de gravaГЦo das tabelas utilizado pelas funГУes GrvItems e GrvGets
Do case
	case nOpc == 3
		cOper := "I"
	case nOpc == 4
		cOper := "A"
	case nOpc == 5
		cOper := "E"
	otherwise
		cOper := "A"
endcase

if nOpcx == 1 //-Confirmou a operaГЦo
	
	//- Define os campos chave de pesquisa e campo de auto incremento para serem utilizados pela rotina de pesquisa da funГЦo GrvItems de cada tabela dos detalhes.
	aChZ0I := {"Z0I_FILIAL+Z0I_CODIGO", xFilial("Z0I")+M->Z0I_CODIGO, "Z0I_ITEM"}
	
	// carrega do cabeГalho os campos e seus respectivos valores que serЦo utilizados na funГЦo de gravaГЦo GrvGets.
	aCpInGet := u_LeAgets(oEnchZ0I:AGETS)
	
	// No grava itens deve-se colocar todos os campos e conteЗdos que se deseja gravar no cabeГalho.
	BEGIN TRANSACTION
	u_GrvItems(cOper,"Z0I",1,aChZ0I,oGDetZ0I:aCols,{{"Z0I_CODIGO",M->Z0I_CODIGO},{"Z0I_DTPGTO",M->Z0I_DTPGTO},{"Z0I_SITIO",M->Z0I_SITIO},{"Z0I_USER",M->Z0I_USER},{"Z0I_DTLANC",M->Z0I_DTLANC},{"Z0I_UNID",M->Z0I_UNID}},aHDetZ0I)
	END TRANSACTION
	
	if nOpc == 3 // incluir
		If ( __lSX8 )
			ConfirmSX8()
		endif
	endif
else //-Cancelou a operaГЦo
	if nOpc == 3 // incluir
		If ( __lSX8 )
			RollBackSX8()
		EndIf
	endif
endif

Return

MsUnLockAll()

Return nil

//-----------------------------------------------------------------------------------------
// LINHA OK - VALIDA LINHA
//-----------------------------------------------------------------------------------------
User Function KsLinZ0I()
//	if !DDMMEntre(BUSCACOLS("Z0T_DATAI"),BUSCACOLS("Z0T_DATAF"),BUSCACOLS("Z0T_CLIMA"))
//	   return(.F.)
//	endif
_lOK := .T.
Return(_lOK)

//-----------------------------------------------------------------------------------------
// VALIDA FIELD (Valida Campos da linha)
//-----------------------------------------------------------------------------------------
User Function KsFieZ0I()
Local _lOKField := .T.
Return(_lOKField)
Return

//-----------------------------------------------------------------------------------------
// TUDO OK - VALIDA TELA INTEIRA ANTES DA INCLUSцO E ALTERAгцO
//-----------------------------------------------------------------------------------------
User Function KsTOkZ0I
Local lOK := .T.

if Val(oSayZ0I03:GetText()) == 0
	msgAlert("Favor incluir ao menos uma linha nos itens!","AtenГЦo!")
	lOK := .F.
endif

Return(lOK)

//------------------------------------------------------
//Mapear os campos do aHeader
//------------------------------------------------------
Static Function MapCampo()
// Variaveis de posicionamento no aCols
_nPosItem	:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2]))=="Z0I_ITEM"})
_nPosDtI 	:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2]))=="Z0I_DATADE"})
_nPosDtF		:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2]))=="Z0I_DATAAT"})
_nPosCodMC	:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2]))=="Z0I_CODMC"})
_nPosDescr	:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2]))=="Z0I_DESC"})
_nPosValor	:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2]))=="Z0I_VALOR"})
_nPosTxExt	:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2]))=="Z0I_EXTRA"})
_nPosDel		:= Len(aHDetZ0I) + 1
Return nil

//-----------------------------------------------------------------------------------------
//AtualizaГЦo do Browser
//-----------------------------------------------------------------------------------------
Static Function fAtuBrowse()

oGDetZ0I:oBrowse:lDisablePaint := .F.
oGDetZ0I:oBrowse:Refresh()

Return

// цдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцддцдцдцдцдцдцдцдцдцдцдцдцдцдц
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor      = JoЦo Anaia Tenero                                                                                                               |
// | Dt.Inicial = 19/07/2010                                                                                                                      |
// | Descricao  = Calcula totais dos itens ao confirmar a tela                                                                                    |
// | Uso        = PFT/BUL                                                                                                                         |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// |                                    A L T E R A C O E S    E F E T U A D A S                                                                  |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor                  | Chamado  | Data       | DescriГЦo                                                                                   |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// |                        |          |            |                                                                                             |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// цдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцддцдцдцдцдцдцдцдцдцдцдцдцдцдц
Static Function CalcTotal()

Local cVlTotal	:= ""
Local nVlTotal := 0
Local cVlTxExt	:= ""
Local nVlTxExt	:= 0
Local cVlTotG	:= ""
Local nVlTotG	:= 0
Local nPVlTotal:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2])) == "Z0I_VALOR"})
Local nPVlTxExt:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2])) == "Z0I_EXTRA"})

For t := 1 to len(oGDetZ0I:Acols)
	IF !oGDetZ0I:Acols[t,Len(aHDetZ0I) + 1] //Soma apenas se o registro nЦo estiver deletado
		nVlTotal	+=oGDetZ0I:Acols[t,nPVlTotal]
		nVlTxExt	+=oGDetZ0I:Acols[t,nPVlTxExt]
	endif
next

nVlTotG	:=nVlTotal+nVlTxExt

cVlTotal	:= SPACE(15) + Transform(nVlTotal,"@E 999,999,999.99")
cVlTxExt	:= SPACE(60) + Transform(nVlTxExt,"@E 999,999,999.99")
cVlTotG	:= SPACE(100) + Transform(nVlTotG,"@E 999,999,999.99")

oGDetZ0I:oBrowse:Refresh()

Return{cVlTotal,cVlTxExt,cVlTotG}

// цдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцддцдцдцдцдцдцдцдцдцдцдцдцдцдц
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor      = JoЦo Anaia Tenero                                                                                                               |
// | Dt.Inicial = 19/07/2010                                                                                                                      |
// | Descricao  = Atualiza os totais ao digitar                                                                                                   |
// | Uso        = PFT/BUL                                                                                                                         |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// |                                    A L T E R A C O E S    E F E T U A D A S                                                                  |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor                  | Chamado  | Data       | DescriГЦo                                                                                   |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// |                        |          |            |                                                                                             |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// цдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцддцдцдцдцдцдцдцдцдцдцдцдцдцдц
User Function KsAtualTot()

Local nVlTotG	:= 0
Local nVlTotal	:= 0
Local nVlTxExt	:= 0
Local nPVlTotG	:= 0
Local nPVlTotal:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2])) == "Z0I_VALOR"})
Local nPVlTxExt:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2])) == "Z0I_EXTRA"})

For t := 1 to len(oGDetZ0I:Acols)
	IF !oGDetZ0I:Acols[t,Len(aHDetZ0I) + 1] //Soma apenas se o registro nЦo estiver deletado
		nVlTotal	+=iif(__ReadVar == 'M->Z0I_VALOR'.and. t==oGDetZ0I:NAT, M->Z0I_VALOR, oGDetZ0I:Acols[t,nPVlTotal]) //ReadVar mostra qual campo esta chamando a funГЦooGDetZ0I:Acols[t,nPVlTotal]
		nVlTxExt	+=iif(__ReadVar == 'M->Z0I_EXTRA'.and. t==oGDetZ0I:NAT, M->Z0I_EXTRA, oGDetZ0I:Acols[t,nPVlTxExt]) //ReadVar mostra qual campo esta chamando a funГЦooGDetZ0I:Acols[t,nPVlTxExt]
	endif
next

nVlTotG	:=nVlTotal+nVlTxExt

//Atualiza os Totais no oPanel
cVlTotG	:= SPACE(100) + Transform(nVlTotG,"@E 999,999,999.99")
cVlTotal	:= SPACE(15) + Transform(nVlTotal,"@E 999,999,999.99")
cVlTxExt	:= SPACE(60) + Transform(nVlTxExt,"@E 999,999,999.99")

oSayZ0I01:SetText(cVlTotal)
oSayZ0I02:SetText(cVlTxExt)
oSayZ0I03:SetText(cVlTotG)
oGDetZ0I:oBrowse:Refresh()

Return .T.

// цдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцддцдцдцдцдцдцдцдцдцдцдцдцдцдц
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor      = JoЦo Anaia Tenero                                                                                                               |
// | Dt.Inicial = 19/07/2010                                                                                                                      |
// | Descricao  = Atualiza os totais ao deletar a linha                                                                                           |
// | Uso        = PFT/BUL                                                                                                                         |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// |                                    A L T E R A C O E S    E F E T U A D A S                                                                  |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor                  | Chamado  | Data       | DescriГЦo                                                                                   |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// |                        |          |            |                                                                                             |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// цдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцдцддцдцдцдцдцдцдцдцдцдцдцдцдцдц
User Function KsDelZ0I()
Local lRet		:= .T.
Local cVlTotG	:= ""
Local nVlTotG	:= 0
Local cVlTotal	:= ""
Local nVlTotal := 0
Local cVlTxExt	:= ""
Local nVlTxExt	:= 0
Local nPVlTotal:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2])) == "Z0I_VALOR"})
Local nPVlTxExt:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2])) == "Z0I_EXTRA"})
Local nPDelete	:= Len(aHDetZ0I)+1

nVlTotal	:=VAL(STRTRAN(STRTRAN(oSayZ0I01:GetText(),'.',''),',','.')) + (oGDetZ0I:Acols[oGDetZ0I:NAT,nPVlTotal] * iif(oGDetZ0I:Acols[oGDetZ0I:NAT,nPDelete],1,-1))
nVlTxExt :=VAL(STRTRAN(STRTRAN(oSayZ0I02:GetText(),'.',''),',','.')) + (oGDetZ0I:Acols[oGDetZ0I:NAT,nPVlTxExt] * iif(oGDetZ0I:Acols[oGDetZ0I:NAT,nPDelete],1,-1))
nVlTotG	:=nVlTotal+nVlTxExt

//Atualiza os Totais no oPanel
cVlTotal	:= SPACE(15) + Transform(nVlTotal,"@E 999,999,999.99")
cVlTxExt	:= SPACE(60) + Transform(nVlTxExt,"@E 999,999,999.99")
cVlTotG	:= SPACE(100) + Transform(nVlTotG,"@E 999,999,999.99")

oSayZ0I01:SetText(cVlTotal)
oSayZ0I02:SetText(cVlTxExt)
oSayZ0I03:SetText(cVlTotG)
oGDetZ0I:oBrowse:Refresh()

Return lRet
