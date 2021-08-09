#include "rwmake.ch"
#include "topconn.ch"
#include "Protheus.ch"

// ������������������������������������������������������������������������������������������������������������������������������������������������
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor      = Felipe Alexandre Soares                                                                                                         |
// | Dt.Inicial = 26/02/2008                                                                                                                      |
// | Descricao  = Contabilizar o rateio do custo do MC do Veiling                                                                                 |
// | Uso        = PFT/BUL                                                                                                                         |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// |                                    A L T E R A C O E S    E F E T U A D A S                                                                  |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor                  | Chamado  | Data       | Descri��o                                                                                   |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// | Felipe A Soares        |          | 03/04/2009 | Inclu�da rotina para n�o deixar alterar quando o pagamento j� foi contabilizado.            |
// |                        |          |            | Inclu�do campo data de lan�amento contabilizado. (Z0I_DTLANC)                               |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// | Jo�o Anaia Ternero     | 00001992 | 19/07/2010 | Alterada a fun��o KsTela3 para Modelo3.                                                     |
// |                        |          |            | Incluidas fun��es de calculo do total no rodap� para os campos Valor, Tx Extra e Total Geral|
// |                        |          |            | Incluida valida��o no TdOk para n�o confirmar a tela se todos os itens estiverem deletados. |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// | David Fernando Pulz    |          | 31/12/2011 | Incluida verifica��o das tabelas exclusivas ou compartilhadas e compara��o da Filial.       |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// | David Fernando Pulz    | P12      | 22/08/2017 | Alterado RegToMemory(Alias, .T., .T. )                                                      |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// ������������������������������������������������������������������������������������������������������������������������������������������������
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
//Visualisa��o,Inclus�o,altera��o e Exclus�o.
//---------------------------------------------------------------------------
User Function KSCZ0I(cAlias,nReg,nOpc)
//-Vari�vel de controle de campos a n�o exibir no cabe�alho, N�O ALTER�-LA.
Local aCpoZ0I := {"NOUSER"}

//-Incluir nesta vari�vel os campos a n�o exibir no cabe�alho
Local aNoInZ0I := {"Z0I_ITEM","Z0I_DATADE","Z0I_DATAAT","Z0I_CODMC","Z0I_DESC","Z0I_VALOR","Z0I_EXTRA"}

//-vari�vel que ser� utilizada para informar o tipo de opera��o de grava��o utlizadas pelas fun��es de grava��o.
Local cOper

//-Matriz que ser� utilizada pela fun��o de grava��o dos detalhes de cada tabela.
Local aChZ0I := {}

//-Vari�vels que conter�o a query de retorno de informa��es para os detalhes.
Local cQryZ0I := ""

//-vari�vel que indica a atualiza��o e redesenho da(s) tela(s) de detalhes
Local lPinta := .T.

//-vari�vel que receber� o objeto da tela principal
Local oDlg

//-vari�vel para guardar a ultima posi��o do registro da tabela principal
Local nRegZ0I := 0

//-vari�veis que definem o m�ximo de itens para cada tabela de detalhes
Local nItZ0I  := 999

//---------------------------------------------------------------------
// Vari�veis para verifica��o de tabelas exclusivas ou compartilhadas
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

//-Vari�vel que receber� o objeto pasta de cabe�alho
Private oPastaCb

//-Vari�vel que receber� o objeto pasta de detalhes
Private oPastaDt

//-vari�veis que receber� o objeto da tela de gets do cabe�alho
Private oEnchZ0I

//-vari�veis para receber o objeto das telas detalhes
Private oGDetZ0I

//-Matriz para adi��o de bot�es na barras de ferramentas
Private aButtons := {}

//-Matriz com titulos das abas do cabe�alho
Private aTitCab := {"&Contabiliza��o do Rateio de Mat. Circulante"}

//-Matriz com titulos das abas dos detalhes
Private aTitDet := {"&Lan�amentos"}

//-vari�vel que receber� os objetos na tela princial, gets do cabe�alho, detalhes, etc...
Private aObjects := {}

//-vari�veis para dimensionamento das telas
Private aInfo    := {}
Private aPosObj  := {}
Private aSize    := MsAdvSize()

//-Matriz principal que recebe informa��es especificas de posicionamento de gets na tela de cabe�alho
Private aTela   := {}

//-uma copia de aTela para cada tabela utilizada no cabe�alho
Private aTela1  := {}

//-Matriz principal que recebe informa��es especificas dos campos utilizados na tela de cabe�alho
Private aGets   := {}

//-vari�vel que receber�o as informa��es das colunas dos detalhes
Private aHeader  := {}
Private aHDetZ0I := {}

Private oSayZ0I01
Private oSayZ0I02
Private oSayZ0I03

//-vari�vel que receber�o os valores dos detalhes
Private aCols 	  := {}
Private aCDetZ0I := {}

//-vari�vel de controle para definir o modo de edi��o das telas
Private _nOpc		:= iif(INCLUI .OR. ALTERA,GD_UPDATE+GD_DELETE+GD_INSERT,0)
Private _nOpcEnch	:= iif(INCLUI .OR. ALTERA,4,5)

//-vari�vel que determina a op��o de confirma��o da tela de lan�amentos, 1 =Ok 2=Cancela.
Private nOpcx := 0

//-adicionando todos os objetos na tela principal "oDlg"
aAdd( aObjects, { 100, 080, .T., .F. } ) //-tela de cabe�alho
aAdd( aObjects, { 100, 170, .T., .F. } ) //-tela de detalhes
AAdd( aObjects, { 100, 015, .T., .T. } ) //-total no rodap�
aInfo   := { aSize[ 1 ], aSize[ 2 ], aSize[ 3 ], aSize[ 4 ], 3, 3 }
aPosObj := MsObjSize( aInfo, aObjects )

//-Posi��es da(s) tela(s) de cabe�alho dentro da pasta de cabe�alho
nPGet1 := 2 //Pos x1
nPGet2 := 2 //Pos y1
nPGet3 := aPosObj[1,3]-aPosObj[1,1]-15 //-pos x2
nPGet4 := aPosObj[1,4]-aPosObj[1,2]-4 //-pos y2

//-Posi��es da(s) tela(s) de detalhes dentro da pasta de detalhes
nGCols1:= 2 //Pos x1
nGCols2:= 2 //Pos y1
nGCols3:= aPosObj[2,3]-aPosObj[2,1]-20 //-pos x2
nGCols4:= aPosObj[2,4]-aPosObj[2,2]-4  //-pos y2

//-Carrega o(s) cabe�alho(s)
If Inclui
	RegToMemory("Z0I", .T., .T. )
	nRegZ0I := 0
Else
	nRegZ0I := Z0I->(Recno())
	RegToMemory("Z0I", .F., .T. )
EndIf

//-determina os campos a exibir no cabe�alho do alias nela informado.
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

// Fun��es de valida��o do cadastro
Private cLnOkZ0I	:= "U_KsLinZ0I()"
Private cTdOkZ0I	:= "U_KsTokZ0I()"
Private cFeOkZ0I	:= "U_KsFieZ0I()"
Private cDelOkZ0I	:= "U_KsDelZ0I()"

// Variaveis de posicionamento no aCols
//MapCampo()

DEFINE MSDIALOG oDlg TITLE cCadastro From aSize[7],00 To aSize[6],aSize[5] OF oMainWnd PIXEL

//-monta a pasta do cabe�alho
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

//- Define o tipo de opera��o de grava��o das tabelas utilizado pelas fun��es GrvItems e GrvGets
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

if nOpcx == 1 //-Confirmou a opera��o
	
	//- Define os campos chave de pesquisa e campo de auto incremento para serem utilizados pela rotina de pesquisa da fun��o GrvItems de cada tabela dos detalhes.
	aChZ0I := {"Z0I_FILIAL+Z0I_CODIGO", xFilial("Z0I")+M->Z0I_CODIGO, "Z0I_ITEM"}
	
	// carrega do cabe�alho os campos e seus respectivos valores que ser�o utilizados na fun��o de grava��o GrvGets.
	aCpInGet := u_LeAgets(oEnchZ0I:AGETS)
	
	// No grava itens deve-se colocar todos os campos e conte�dos que se deseja gravar no cabe�alho.
	BEGIN TRANSACTION
	u_GrvItems(cOper,"Z0I",1,aChZ0I,oGDetZ0I:aCols,{{"Z0I_CODIGO",M->Z0I_CODIGO},{"Z0I_DTPGTO",M->Z0I_DTPGTO},{"Z0I_SITIO",M->Z0I_SITIO},{"Z0I_USER",M->Z0I_USER},{"Z0I_DTLANC",M->Z0I_DTLANC},{"Z0I_UNID",M->Z0I_UNID}},aHDetZ0I)
	END TRANSACTION
	
	if nOpc == 3 // incluir
		If ( __lSX8 )
			ConfirmSX8()
		endif
	endif
else //-Cancelou a opera��o
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
// TUDO OK - VALIDA TELA INTEIRA ANTES DA INCLUS�O E ALTERA��O
//-----------------------------------------------------------------------------------------
User Function KsTOkZ0I
Local lOK := .T.

if Val(oSayZ0I03:GetText()) == 0
	msgAlert("Favor incluir ao menos uma linha nos itens!","Aten��o!")
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
//Atualiza��o do Browser
//-----------------------------------------------------------------------------------------
Static Function fAtuBrowse()

oGDetZ0I:oBrowse:lDisablePaint := .F.
oGDetZ0I:oBrowse:Refresh()

Return

// ������������������������������������������������������������������������������������������������������������������������������������������������
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor      = Jo�o Anaia Tenero                                                                                                               |
// | Dt.Inicial = 19/07/2010                                                                                                                      |
// | Descricao  = Calcula totais dos itens ao confirmar a tela                                                                                    |
// | Uso        = PFT/BUL                                                                                                                         |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// |                                    A L T E R A C O E S    E F E T U A D A S                                                                  |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor                  | Chamado  | Data       | Descri��o                                                                                   |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// |                        |          |            |                                                                                             |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// ������������������������������������������������������������������������������������������������������������������������������������������������
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
	IF !oGDetZ0I:Acols[t,Len(aHDetZ0I) + 1] //Soma apenas se o registro n�o estiver deletado
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

// ������������������������������������������������������������������������������������������������������������������������������������������������
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor      = Jo�o Anaia Tenero                                                                                                               |
// | Dt.Inicial = 19/07/2010                                                                                                                      |
// | Descricao  = Atualiza os totais ao digitar                                                                                                   |
// | Uso        = PFT/BUL                                                                                                                         |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// |                                    A L T E R A C O E S    E F E T U A D A S                                                                  |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor                  | Chamado  | Data       | Descri��o                                                                                   |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// |                        |          |            |                                                                                             |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// ������������������������������������������������������������������������������������������������������������������������������������������������
User Function KsAtualTot()

Local nVlTotG	:= 0
Local nVlTotal	:= 0
Local nVlTxExt	:= 0
Local nPVlTotG	:= 0
Local nPVlTotal:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2])) == "Z0I_VALOR"})
Local nPVlTxExt:= aScan(aHDetZ0I,{|x| AllTrim(Upper(x[2])) == "Z0I_EXTRA"})

For t := 1 to len(oGDetZ0I:Acols)
	IF !oGDetZ0I:Acols[t,Len(aHDetZ0I) + 1] //Soma apenas se o registro n�o estiver deletado
		nVlTotal	+=iif(__ReadVar == 'M->Z0I_VALOR'.and. t==oGDetZ0I:NAT, M->Z0I_VALOR, oGDetZ0I:Acols[t,nPVlTotal]) //ReadVar mostra qual campo esta chamando a fun��ooGDetZ0I:Acols[t,nPVlTotal]
		nVlTxExt	+=iif(__ReadVar == 'M->Z0I_EXTRA'.and. t==oGDetZ0I:NAT, M->Z0I_EXTRA, oGDetZ0I:Acols[t,nPVlTxExt]) //ReadVar mostra qual campo esta chamando a fun��ooGDetZ0I:Acols[t,nPVlTxExt]
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

// ������������������������������������������������������������������������������������������������������������������������������������������������
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor      = Jo�o Anaia Tenero                                                                                                               |
// | Dt.Inicial = 19/07/2010                                                                                                                      |
// | Descricao  = Atualiza os totais ao deletar a linha                                                                                           |
// | Uso        = PFT/BUL                                                                                                                         |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// |                                    A L T E R A C O E S    E F E T U A D A S                                                                  |
// |----------------------------------------------------------------------------------------------------------------------------------------------|
// | Autor                  | Chamado  | Data       | Descri��o                                                                                   |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// |                        |          |            |                                                                                             |
// |------------------------+----------+------------+---------------------------------------------------------------------------------------------|
// ������������������������������������������������������������������������������������������������������������������������������������������������
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
