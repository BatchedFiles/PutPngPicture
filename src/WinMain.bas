#include once "WinMain.bi"
#include once "win\commctrl.bi"
#include once "win\commdlg.bi"
#include once "win\shellapi.bi"
#include once "win\windowsx.bi"
#include once "win\mswsock.bi"
#include once "Base64.bi"
#include once "Registry.bi"
#include once "Resources.RH"
#include once "Win95Hack.bi"

Const NETEVENT_NOTICE = WM_USER + 2

Enum NetEventKind
	Connect
	SendHeaders
	StepsCount
	NextStep
	SendBody
	ReadResponse
	Done
End Enum

Enum RequestHeaders
	HeaderAccept
	HeaderAuthorization
	HeaderConnection
	HeaderContentLength
	HeaderContentType
	HeaderExpect
	HeaderHost
	HeaderUserAgent
End Enum

Const RequestHeadersLength = 8

Type ClientRequest
	RequestLine As TCHAR Ptr
	ServerAddress As HOSTENT Ptr
	ServerPort As Integer
	AllHeaders As TCHAR Ptr
	AllHeadersLength As Integer
	Headers(RequestHeadersLength - 1) As TCHAR Ptr
End Type

Enum FileType
	DiskFile
	Temporary
End Enum

Type HttpRestForm
	hInst As HINSTANCE
	FileHandle As HANDLE
	MapFileHandle As HANDLE
	ClientSocket As SOCKET
	liFileSize As LARGE_INTEGER
	IsTemporaryFile As FileType
	hHeap As HANDLE
	hEvent As HANDLE
	hWin As HWND
	pResponseMem As ZString Ptr
	ResponseLength As Integer
	CRequest As ClientRequest
End Type

Type VerbsVector
	Verbs(6) As TCHAR Ptr
End Type

Const RESOURCE_STRING_BUFFER_CAPACITY = 255

Type ResourceStringBuffer
	szText(RESOURCE_STRING_BUFFER_CAPACITY) As TCHAR
End Type

Type FileNameBuffer
	szText(MAX_PATH) As TCHAR
End Type

Type ErrorBuffer
	szText(RESOURCE_STRING_BUFFER_CAPACITY) As TCHAR
End Type

Type HttpRestFormSettings
	Server As FileNameBuffer
	Resource As FileNameBuffer
	Verb As FileNameBuffer
	File As FileNameBuffer
	ContentType As FileNameBuffer
End Type

Private Function HeaderNameToString( _
		ByVal Index As RequestHeaders _
	)As TCHAR Ptr

	Select Case Index

		Case RequestHeaders.HeaderAccept
			Const HeaderString = __TEXT("Accept")
			Return @HeaderString

		Case RequestHeaders.HeaderAuthorization
			Const HeaderString = __TEXT("Authorization")
			Return @HeaderString

		Case RequestHeaders.HeaderConnection
			Const HeaderString = __TEXT("Connection")
			Return @HeaderString

		Case RequestHeaders.HeaderContentLength
			Const HeaderString = __TEXT("Content-Length")
			Return @HeaderString

		Case RequestHeaders.HeaderContentType
			Const HeaderString = __TEXT("Content-Type")
			Return @HeaderString

		Case RequestHeaders.HeaderExpect
			Const HeaderString = __TEXT("Expect")
			Return @HeaderString

		Case RequestHeaders.HeaderHost
			Const HeaderString = __TEXT("Host")
			Return @HeaderString

		Case RequestHeaders.HeaderUserAgent
			Const HeaderString = __TEXT("User-Agent")
			Return @HeaderString

		Case Else
			Return NULL

	End Select

End Function

Private Sub RestHeadersCleanup( _
		ByVal this As HttpRestForm Ptr _
	)

	If this->CRequest.RequestLine Then
		HeapFree(this->hHeap, 0, this->CRequest.RequestLine)
		this->CRequest.RequestLine = NULL
	End If

	For i As Integer = 0 To RequestHeadersLength - 1
		If this->CRequest.Headers(i) Then
			HeapFree(this->hHeap, 0, this->CRequest.Headers(i))
			this->CRequest.Headers(i) = NULL
		End If
	Next

End Sub

Private Sub HttpRestFormCleanUp( _
		ByVal this As HttpRestForm Ptr _
	)

	If this->ClientSocket <> INVALID_SOCKET Then
		shutdown(this->ClientSocket, SD_BOTH)
		closesocket(this->ClientSocket)
		this->ClientSocket = INVALID_SOCKET
	End If

	If this->MapFileHandle Then
		CloseHandle(this->MapFileHandle)
		this->MapFileHandle = NULL
	End If

	If this->FileHandle <> INVALID_HANDLE_VALUE Then
		CloseHandle(this->FileHandle)
		this->FileHandle = INVALID_HANDLE_VALUE
		this->liFileSize.QuadPart = 0
	End If

	If this->CRequest.AllHeaders Then
		HeapFree(this->hHeap, 0, this->CRequest.AllHeaders)
		this->CRequest.AllHeaders = NULL
	End If

	RestHeadersCleanup(this)

End Sub

Private Function div64( _
		ByVal Divisible As ULongInt, _
		ByVal Divisor As ULongInt _
	) As ULongInt

	Dim Partial As ULongInt = 0

	Dim tmpDivisible As ULongInt = Divisible

	Do While tmpDivisible >= Divisor
		tmpDivisible -= Divisor
		Partial += 1
	Loop

	Return Partial

End Function

Private Function idiv64( _
		ByVal Divisible As LongInt, _
		ByVal Divisor As LongInt _
	) As LongInt

	If Divisor < 0 Then
		Dim Partial As LongInt = idiv64(Divisible, -1 * Divisor)
		Return -1 * Partial
	End If

	If Divisible < 0 Then
		Dim Partial As LongInt = idiv64(-1 * Divisible, Divisor)
		Return -1 * Partial
	End If

	Return div64(Divisible, -Divisor)

End Function

Private Sub HttpRestFormToString( _
		ByVal this As HttpRestForm Ptr _
	)

	Const PageSize As Integer = 4096

	this->CRequest.AllHeadersLength = 0

	Dim cbAllHeaders As Integer = (PageSize) * 8
	this->CRequest.AllHeaders = HeapAlloc( _
		this->hHeap, _
		0, _
		cbAllHeaders _
	)
	If this->CRequest.AllHeaders = NULL Then
		Exit Sub
	End If

	Const FormatString = __TEXT(!"%s\r\n")

	Dim Length As Long = wsprintf( _
		@this->CRequest.AllHeaders[this->CRequest.AllHeadersLength], _
		@FormatString, _
		this->CRequest.RequestLine _
	)
	this->CRequest.AllHeadersLength += Length

	For i As Integer = 0 To RequestHeadersLength - 2
		Dim HeaderValue As TCHAR Ptr = this->CRequest.Headers(i)

		If HeaderValue Then
			Const FormatString2 = __TEXT(!"%s: %s\r\n")

			Dim HeaderName As TCHAR Ptr = HeaderNameToString(i)

			Dim Length2 As Long = wsprintf( _
				@this->CRequest.AllHeaders[this->CRequest.AllHeadersLength], _
				@FormatString2, _
				HeaderName, _
				HeaderValue _
			)
			this->CRequest.AllHeadersLength += Length2
		End If
	Next

	Scope
		Dim HeaderValue As TCHAR Ptr = this->CRequest.Headers(RequestHeaders.HeaderUserAgent)

		If HeaderValue Then
			Const FormatString3 = __TEXT(!"%s: %s\r\n\r\n")

			Dim HeaderName As TCHAR Ptr = HeaderNameToString(RequestHeaders.HeaderUserAgent)

			Dim Length3 As Long = wsprintf( _
				@this->CRequest.AllHeaders[this->CRequest.AllHeadersLength], _
				@FormatString3, _
				HeaderName, _
				HeaderValue _
			)
			this->CRequest.AllHeadersLength += Length3
		End If
	End Scope

	' Truncate bloat memory

	RestHeadersCleanup(this)

	Dim pMem As Any Ptr = HeapReAlloc( _
		this->hHeap, _
		0, _
		this->CRequest.AllHeaders, _
		this->CRequest.AllHeadersLength _
	)

	If pMem Then
		this->CRequest.AllHeaders = pMem
	End If

End Sub

Private Function HttpRestFormSendFile( _
		ByVal this As HttpRestForm Ptr _
	)As HRESULT

	If this->MapFileHandle = NULL Then
		Return S_OK
	End If

	Dim PieceSize As LARGE_INTEGER = Any

	Scope
		Dim SmallPiecesCount As LARGE_INTEGER = Any
		SmallPiecesCount.QuadPart = idiv64(this->liFileSize.QuadPart, 65536)

		If SmallPiecesCount.QuadPart = 0 Then
			SmallPiecesCount.QuadPart = 1
		End If

		Dim LargePiecesCount As LARGE_INTEGER = Any
		LargePiecesCount.QuadPart = idiv64(SmallPiecesCount.QuadPart, 65536)

		If LargePiecesCount.QuadPart = 0 Then
			LargePiecesCount.QuadPart = 1
		End If

		PieceSize.QuadPart = LargePiecesCount.QuadPart * 65536
	End Scope

	Scope
		Dim stpCount As UInteger = idiv64(this->liFileSize.QuadPart, PieceSize.QuadPart)
		Dim stpCount1 As UInteger = stpCount + 1

		PostMessage( _
			this->hWin, _
			NETEVENT_NOTICE, _
			NetEventKind.StepsCount, _
			stpCount1 _
		)
	End Scope

	Dim Offset As LARGE_INTEGER = Any
	Offset.QuadPart = 0

	Do
		Dim Tail As LARGE_INTEGER = Any
		Tail.QuadPart = this->liFileSize.QuadPart - Offset.QuadPart

		Dim cbBytes As DWORD = min(PieceSize.QuadPart, Tail.QuadPart)

		If cbBytes = 0 Then
			Exit Do
		End If

		Dim p As UByte Ptr = MapViewOfFile( _
			this->MapFileHandle, _
			FILE_MAP_READ, _
			Offset.HighPart, Offset.LowPart, _
			cbBytes _
		)
		If p = NULL Then
			Return HRESULT_FROM_WIN32(ERROR_NOT_ENOUGH_MEMORY)
		End If

		Dim resSend As Long = send( _
			this->ClientSocket, _
			p, _
			cbBytes, _
			0 _
		)
		If resSend = SOCKET_ERROR Then
			Dim dwError As Long = WSAGetLastError()
			UnmapViewOfFile(p)

			Return HRESULT_FROM_WIN32(dwError)
		End If

		UnmapViewOfFile(p)

		PostMessage( _
			this->hWin, _
			NETEVENT_NOTICE, _
			NetEventKind.NextStep, _
			ERROR_SUCCESS _
		)

		If cbBytes < PieceSize.QuadPart Then
			Exit Do
		End If

		Offset.QuadPart = Offset.QuadPart + PieceSize.QuadPart

	Loop

	Return S_OK

End Function

Private Function WorkerThread( _
		ByVal lpParam As LPVOID _
	)As DWORD

	Dim this As HttpRestForm Ptr = lpParam

	Do
		Dim resWait As DWORD = WaitForSingleObject( _
			this->hEvent, _
			INFINITE _
		)

		Select Case resWait

			Case WAIT_OBJECT_0
				ResetEvent(this->hEvent)

				' Connect to Server
				Scope
					Dim DestinationIpEndPoint As SOCKADDR_IN = Any
					ZeroMemory(@DestinationIpEndPoint, SizeOf(SOCKADDR_IN))

					DestinationIpEndPoint.sin_family = AF_INET
					Dim ShortPort As UShort = CUShort(this->CRequest.ServerPort)
					DestinationIpEndPoint.sin_port = htons(ShortPort)

					MoveMemory( _
						@DestinationIpEndPoint.sin_addr, _
						this->CRequest.ServerAddress->h_addr, _
						this->CRequest.ServerAddress->h_length _
					)

					Dim psaddr As SOCKADDR Ptr = CPtr(SOCKADDR Ptr, @DestinationIpEndPoint)
					Dim Size As Integer = SizeOf(SOCKADDR_IN)

					Dim resConnect As Long = connect(this->ClientSocket, psaddr, Size)
					If resConnect = SOCKET_ERROR Then
						Dim dwError As Long = WSAGetLastError()

						HttpRestFormCleanUp(this)

						PostMessage( _
							this->hWin, _
							NETEVENT_NOTICE, _
							NetEventKind.Connect, _
							dwError _
						)
						Continue Do
					End If

					PostMessage( _
						this->hWin, _
						NETEVENT_NOTICE, _
						NetEventKind.Connect, _
						ERROR_SUCCESS _
					)
				End Scope

				' Send Headers
				Scope
					Dim resSend As Long = send( _
						this->ClientSocket, _
						this->CRequest.AllHeaders, _
						this->CRequest.AllHeadersLength, _
						0 _
					)
					If resSend = SOCKET_ERROR Then
						Dim dwError As Long = WSAGetLastError()

						HttpRestFormCleanUp(this)

						PostMessage( _
							this->hWin, _
							NETEVENT_NOTICE, _
							NetEventKind.SendHeaders, _
							dwError _
						)
						Continue Do
					End If

					PostMessage( _
						this->hWin, _
						NETEVENT_NOTICE, _
						NetEventKind.SendHeaders, _
						ERROR_SUCCESS _
					)
				End Scope

				' Send Body
				Scope
					Dim hrSend As HRESULT = HttpRestFormSendFile(this)
					If FAILED(hrSend) Then
						HttpRestFormCleanUp(this)

						PostMessage( _
							this->hWin, _
							NETEVENT_NOTICE, _
							NetEventKind.SendBody, _
							hrSend _
						)
						Continue Do
					End If

					PostMessage( _
						this->hWin, _
						NETEVENT_NOTICE, _
						NetEventKind.SendBody, _
						ERROR_SUCCESS _
					)
				End Scope

				' Read Response from Server
				Scope
					Const RESPONSE_MEM_SIZE = 65536 - 1

					this->pResponseMem = HeapAlloc( _
						this->hHeap, _
						0, _
						RESPONSE_MEM_SIZE _
					)
					If this->pResponseMem = NULL Then
						HttpRestFormCleanUp(this)

						PostMessage( _
							this->hWin, _
							NETEVENT_NOTICE, _
							NetEventKind.ReadResponse, _
							ERROR_NOT_ENOUGH_MEMORY _
						)
					End If

					this->ResponseLength = recv( _
						this->ClientSocket, _
						this->pResponseMem, _
						RESPONSE_MEM_SIZE, _
						0 _
					)
					If this->ResponseLength = SOCKET_ERROR Then
						Dim dwError As Long = WSAGetLastError()

						HeapFree( _
							this->hHeap, _
							0, _
							this->pResponseMem _
						)

						this->ResponseLength = 0
						this->pResponseMem = NULL

						HttpRestFormCleanUp(this)

						PostMessage( _
							this->hWin, _
							NETEVENT_NOTICE, _
							NetEventKind.ReadResponse, _
							dwError _
						)
						Continue Do
					End If

					' Set Terminate NUL Character
					this->pResponseMem[this->ResponseLength] = 0

					PostMessage( _
						this->hWin, _
						NETEVENT_NOTICE, _
						NetEventKind.ReadResponse, _
						0 _
					)

				End Scope

				' Sending file done
				Scope
					HttpRestFormCleanUp(this)

					PostMessage( _
						this->hWin, _
						NETEVENT_NOTICE, _
						NetEventKind.Done, _
						0 _
					)
				End Scope

			Case Else
				Exit Do

		End Select

	Loop

	Return 0

End Function

Private Sub DisplayError( _
		ByVal hInst As HINSTANCE, _
		ByVal hWin As HWND, _
		ByVal dwErrorCode As DWORD, _
		ByVal CaptionId As UINT _
	)

	Dim FormatString As ResourceStringBuffer = Any
	LoadString( _
		hInst, _
		IDS_ERRORFORMAT, _
		@FormatString.szText(0), _
		RESOURCE_STRING_BUFFER_CAPACITY _
	)

	Dim buf As ErrorBuffer = Any
	Dim BufLen As Integer = wsprintf( _
		@buf.szText(0), _
		@FormatString.szText(0), _
		dwErrorCode _
	)

	If BufLen Then
		Dim wBuffer As ErrorBuffer = Any
		Dim BufLen2 As Integer = (RESOURCE_STRING_BUFFER_CAPACITY - 1) - BufLen
		Dim CharsCount As DWORD = FormatMessage( _
			FORMAT_MESSAGE_FROM_SYSTEM Or FORMAT_MESSAGE_MAX_WIDTH_MASK, _
			NULL, _
			dwErrorCode, _
			0, _
			@buf.szText(BufLen), _
			BufLen2, _
			NULL _
		)

		If CharsCount Then
			Dim MsgCaption As ResourceStringBuffer = Any
			LoadString( _
				hInst, _
				CaptionId, _
				@MsgCaption.szText(0), _
				RESOURCE_STRING_BUFFER_CAPACITY _
			)
			MessageBox( _
				hWin, _
				@buf.szText(0), _
				@MsgCaption.szText(0), _
				MB_OK Or MB_ICONERROR _
			)
		End If
	End If

End Sub

Private Sub DisplaySuccess( _
		ByVal hInst As HINSTANCE, _
		ByVal hWin As HWND, _
		ByVal CaptionId As UINT, _
		ByVal MsgTextId As UINT _
	)

	Dim MsgText As ResourceStringBuffer = Any
	LoadString( _
		hInst, _
		MsgTextId, _
		@MsgText.szText(0), _
		RESOURCE_STRING_BUFFER_CAPACITY _
	)

	Dim MsgCaption As ResourceStringBuffer = Any
	LoadString( _
		hInst, _
		CaptionId, _
		@MsgCaption.szText(0), _
		RESOURCE_STRING_BUFFER_CAPACITY _
	)

	MessageBox( _
		hWin, _
		@MsgText.szText(0), _
		@MsgCaption.szText(0), _
		MB_OK Or MB_ICONINFORMATION _
	)

End Sub

Private Sub DisableDialogItem( _
		ByVal hWin As HWND, _
		ByVal Id As UINT _
	)

	SendMessage(hWin, WM_NEXTDLGCTL, 0, 0)

	Dim hwndOk As HWND = GetDlgItem(hWin, Id)
	EnableWindow(hwndOk, False)

End Sub

Private Sub EnableDialogItem( _
		ByVal hWin As HWND, _
		ByVal Id As UINT _
	)

	Dim hwndOk As HWND = GetDlgItem(hWin, Id)
	EnableWindow(hwndOk, True)

End Sub

Private Function StringToInteger( _
		ByVal pBuffer As TCHAR Ptr _
	)As Integer

	Dim Number As Integer = 0
	Dim i As Integer = Any
	Dim nuSign As UInteger = Any

	If pBuffer[0] = Asc("-") Then
		nuSign = -1
		i = 1
	Else
		nuSign = 0
		i = 0
	End If

	Do While pBuffer[i] >= &h30 AndAlso pBuffer[i] <= &h39
		Dim Digit As Integer = pBuffer[i] And &h0F
		Number = Number + Digit
		Number = Number * 10
		i += 1
	Loop

	Number = Number \ 10

	If nuSign Then
		Return -1 * Number
	End If

	Return Number

End Function

Private Function SplitServerPort( _
		ByVal pszText As TCHAR Ptr, _
		ByVal Length As Integer _
	)As Integer

	Dim pszPort As TCHAR Ptr = NULL

	For i As Integer = 0 To Length - 1
		Dim Ch As Integer = pszText[i]
		If Ch = Asc(":") Then
			pszText[i] = 0
			Dim nNext As Integer = i + 1
			pszPort = @pszText[nNext]
			Exit For
		End If
	Next

	Const HttpPort As Integer = 80

	If pszPort Then
		Dim PortLength As Long = lstrlen(pszPort)
		If PortLength Then
			Dim Port As Integer = StringToInteger(pszPort)
			Return Port
		Else
			Return HttpPort
		End If
	Else
		Return HttpPort
	End If

End Function

Private Function NetworkStartUp()As HRESULT

	Dim wsa As WSADATA = Any

	Dim WsaVersion22 As WORD = MAKEWORD(1, 1)
	Dim resWsaStartup As Long = WSAStartup(WsaVersion22, @wsa)

	If resWsaStartup <> NO_ERROR Then
		Dim dwError As Long = WSAGetLastError()
		Return HRESULT_FROM_WIN32(dwError)
	End If

	Return S_OK

End Function

Private Function NetworkCleanUp()As HRESULT

	Dim resStartup As Long = WSACleanup()
	If resStartup <> 0 Then
		Dim dwError As Long = WSAGetLastError()
		Return HRESULT_FROM_WIN32(dwError)
	End If

	Return S_OK

End Function

Private Sub Socket_OnWSANetEvent( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND, _
		ByVal nEvent As Integer, _
		ByVal nError As Integer _
	)

	Select Case nEvent

		Case NetEventKind.Connect
			If nError Then
				DisplayError(this->hInst, hWin, nError, IDS_CONNECT)
				EndDialog(hWin, IDCANCEL)
			End If

		Case NetEventKind.SendHeaders
			If nError Then
				DisplayError(this->hInst, hWin, nError, IDS_SENDHEADERS)
				EndDialog(hWin, IDCANCEL)
			End If

		Case NetEventKind.StepsCount
			SendDlgItemMessage( _
				hWin, _
				IDC_PRB_PROGRESS, _
				PBM_SETRANGE, _
				0, _
				MAKELPARAM(1, nError) _
			)
			SendDlgItemMessage( _
				hWin, _
				IDC_PRB_PROGRESS, _
				PBM_SETSTEP, _
				1, _
				0 _
			)

		Case NetEventKind.NextStep
			SendDlgItemMessage( _
				hWin, _
				IDC_PRB_PROGRESS, _
				PBM_STEPIT, _
				0, _
				0 _
			)

		Case NetEventKind.SendBody
			If nError Then
				DisplayError(this->hInst, hWin, nError, IDS_SENDBODY)
				EndDialog(hWin, IDCANCEL)
			End If

		Case NetEventKind.ReadResponse
			If nError Then
				DisplayError(this->hInst, hWin, nError, IDS_READRESPONSE)
				EndDialog(hWin, IDCANCEL)
			End If

		Case NetEventKind.Done
			DisplaySuccess( _
				this->hInst, _
				hWin, _
				IDS_DONE, _
				IDS_SENDINGDONE _
			)
			EndDialog(hWin, IDOK)

	End Select

End Sub

Private Sub IDCANCEL2_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	EndDialog(hWin, IDCANCEL)

End Sub

Private Sub DialogProgress_OnLoad( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	this->hWin = hWin
	SetEvent(this->hEvent)

End Sub

Private Sub DialogProgress_OnUnload( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	EndDialog(hWin, IDCANCEL)

End Sub

Private Function ProgressDialogProc( _
		ByVal hWin As HWND, _
		ByVal uMsg As UINT, _
		ByVal wParam As WPARAM, _
		ByVal lParam As LPARAM _
	)As INT_PTR

	Dim pParam As HttpRestForm Ptr = Any

	If uMsg = WM_INITDIALOG Then
		pParam = Cast(HttpRestForm Ptr, lParam)
		DialogProgress_OnLoad(pParam, hWin)
		SetWindowLongPtr(hWin, GWLP_USERDATA, Cast(LONG_PTR, pParam))

		Return True
	End If

	pParam = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))

	Select Case uMsg

		Case WM_COMMAND

			Dim Reason As WORD = HIWORD(wParam)
			Dim ControlId As WORD = LOWORD(wParam)

			Select Case Reason

				Case BN_CLICKED
					' Menu or Button

					Select Case ControlId

						Case IDCANCEL
							IDCANCEL2_OnClick(pParam, hWin)

						Case Else
							Return False

					End Select

				Case 1
					' Accelerator
					Return False

				Case Else
					Return False

			End Select

		Case NETEVENT_NOTICE
			Dim nEvent As Integer = wParam
			Dim nError As Integer = CInt(lParam)
			Socket_OnWSANetEvent(pParam, hWin, nEvent, nError)

		Case WM_CLOSE
			DialogProgress_OnUnload(pParam, hWin)

		Case Else
			Return FALSE

	End Select

	Return TRUE

End Function

Private Sub FillSettingsVector( _
		ByVal pVec As SettingsVector Ptr, _
		ByVal pMem As HttpRestFormSettings Ptr _
	)

	pVec->Vec(0).Key = @ServerKeyString
	pVec->Vec(0).Value = @pMem->Server.szText(0)
	pVec->Vec(0).ControlId = IDC_EDT_SERVER

	pVec->Vec(1).Key = @ResourceKeyString
	pVec->Vec(1).Value = @pMem->Resource.szText(0)
	pVec->Vec(1).ControlId = IDC_EDT_RESOURCE

	pVec->Vec(2).Key = @VerbKeyString
	pVec->Vec(2).Value = @pMem->Verb.szText(0)
	pVec->Vec(2).ControlId = IDC_CBB_VERB

	pVec->Vec(3).Key = @FileKeyString
	pVec->Vec(3).Value = @pMem->File.szText(0)
	pVec->Vec(3).ControlId = IDC_EDT_FILE

	pVec->Vec(4).Key = @ContentTypeKeyString
	pVec->Vec(4).Value = @pMem->ContentType.szText(0)
	pVec->Vec(4).ControlId = IDC_EDT_TYPE

End Sub

Private Sub FillVerbsVector( _
		ByVal vecVerbs As VerbsVector Ptr _
	)

	Const PutVerbString = __TEXT("PUT")
	Const GetVerbString = __TEXT("GET")
	Const HeadVerbString = __TEXT("HEAD")
	Const TraceVerbString = __TEXT("TRACE")
	Const OptionsVerbString = __TEXT("OPTIONS")
	Const DeleteVerbString = __TEXT("DELETE")
	Const PostVerbString = __TEXT("POST")

	vecVerbs->Verbs(0) = @PutVerbString
	vecVerbs->Verbs(1) = @GetVerbString
	vecVerbs->Verbs(2) = @HeadVerbString
	vecVerbs->Verbs(3) = @TraceVerbString
	vecVerbs->Verbs(4) = @OptionsVerbString
	vecVerbs->Verbs(5) = @DeleteVerbString
	vecVerbs->Verbs(6) = @PostVerbString

End Sub

Private Sub DialogMain_OnLoad( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	this->hWin = hWin

	DragAcceptFiles(hWin, TRUE)

	Scope
		Dim hcn As HICON = LoadIcon(this->hInst, MAKEINTRESOURCE(IDI_MAIN))
		SendMessage(hWin, WM_SETICON, ICON_SMALL, Cast(LPARAM, hcn))
		SendMessage(hWin, WM_SETICON, ICON_BIG, Cast(LPARAM, hcn))
	End Scope

	Scope
		Dim hwndCombo As HWND = GetDlgItem(hWin, IDC_CBB_VERB)

		Dim vecVerbs As VerbsVector = Any
		FillVerbsVector(@vecVerbs)

		For i As Integer = LBound(vecVerbs.Verbs) To UBound(vecVerbs.Verbs)
			Dim p As TCHAR Ptr = vecVerbs.Verbs(i)
			ComboBox_AddString(hwndCombo, p)
		Next

		ComboBox_SetCurSel(hwndCombo, 0)
	End Scope

	Scope
		Dim pMem As HttpRestFormSettings Ptr = HeapAlloc( _
			this->hHeap, _
			0, _
			SizeOf(HttpRestFormSettings) _
		)

		If pMem Then
			Dim vec As SettingsVector = Any
			FillSettingsVector(@vec, pMem)

			Dim hrLoad As HRESULT = LoadSettings(@vec)

			If SUCCEEDED(hrLoad) Then
				For i As Integer = LBound(vec.Vec) To UBound(vec.Vec)

					If vec.Vec(i).ValueLength Then
						SetDlgItemText( _
							hWin, _
							vec.Vec(i).ControlId, _
							vec.Vec(i).Value _
						)
					End If
				Next
			End If

			HeapFree( _
				this->hHeap, _
				0, _
				pMem _
			)
		End If
	End Scope

End Sub

Private Sub IDCANCEL_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	EndDialog(hWin, IDCANCEL)

End Sub

Private Sub IDOK_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	' Get Parameters from Dialog
	Scope
		' Create RequestLine
		Scope
			Const FormatString = __TEXT(!"%s %s HTTP/1.1")

			Dim bufVerb As FileNameBuffer = Any
			Dim VerbLength As Long = GetDlgItemText( _
				hWin, _
				IDC_CBB_VERB, _
				@bufVerb.szText(0), _
				MAX_PATH _
			)

			Dim bufUrlParh As FileNameBuffer = Any
			Dim UrlParhLength As Long = GetDlgItemText( _
				hWin, _
				IDC_EDT_RESOURCE, _
				@bufUrlParh.szText(0), _
				MAX_PATH _
			)

			If VerbLength = 0 OrElse UrlParhLength = 0 Then
				HttpRestFormCleanUp(this)
				DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_METHODMUSTBE)
				Exit Sub
			End If

			Dim bufRequestLine As FileNameBuffer = Any
			Dim Length As Long = wsprintf( _
				@bufRequestLine.szText(0), _
				@FormatString, _
				@bufVerb.szText(0), _
				@bufUrlParh.szText(0) _
			)

			Dim cbBytesUrlParh As Integer = (Length + 1) * SizeOf(TCHAR)
			this->CRequest.RequestLine = HeapAlloc( _
				this->hHeap, _
				0, _
				cbBytesUrlParh _
			)
			If this->CRequest.RequestLine = NULL Then
				HttpRestFormCleanUp(this)
				DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTEDLINE)
				Exit Sub
			End If

			lstrcpy( _
				this->CRequest.RequestLine, _
				@bufRequestLine.szText(0) _
			)
		End Scope

		' Create Accept Header
		Scope
			Const AcceptString = __TEXT("*/*")
			Dim cbAcceptString As Integer = (Len(AcceptString) + 1) * SizeOf(TCHAR)
			this->CRequest.Headers(RequestHeaders.HeaderAccept) = HeapAlloc( _
				this->hHeap, _
				0, _
				cbAcceptString _
			)
			If this->CRequest.Headers(RequestHeaders.HeaderAccept) = NULL Then
				HttpRestFormCleanUp(this)
				DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTHEADER)
				Exit Sub
			End If

			lstrcpy( _
				this->CRequest.Headers(RequestHeaders.HeaderAccept), _
				@AcceptString _
			)
		End Scope

		' Create Authorization Header
		Scope
			Dim bufUserName As FileNameBuffer = Any
			Dim UserNameLength As Long = GetDlgItemText( _
				hWin, _
				IDC_EDT_USER, _
				@bufUserName.szText(0), _
				MAX_PATH _
			)

			If UserNameLength Then
				Dim bufPassword As FileNameBuffer = Any
				Dim PasswordLength As Long = GetDlgItemText( _
					hWin, _
					IDC_EDT_PASSWORD, _
					@bufPassword.szText(0), _
					MAX_PATH _
				)

				If PasswordLength Then

					Dim bufUserPasswordBase64 As FileNameBuffer = Any

					Scope
						Dim bufUserCommaPassword As FileNameBuffer = Any

						Const FormatString = __TEXT(!"%s:%s")

						Dim Length As Long = wsprintf( _
							@bufUserCommaPassword.szText(0), _
							@FormatString, _
							@bufUserName.szText(0), _
							@bufPassword.szText(0) _
						)

						Encode64( _
							@bufUserCommaPassword.szText(0), _
							Length, _
							@bufUserPasswordBase64.szText(0) _
						)
					End Scope

					Scope
						Dim bufBasicUserPassword As FileNameBuffer = Any

						Const FormatString = __TEXT(!"Basic %s")

						Dim Length As Long = wsprintf( _
							@bufBasicUserPassword.szText(0), _
							@FormatString, _
							@bufUserPasswordBase64.szText(0) _
						)

						Dim cbBytesAuthorization As Integer = (Length + 1) * SizeOf(TCHAR)
						this->CRequest.Headers(RequestHeaders.HeaderAuthorization) = HeapAlloc( _
							this->hHeap, _
							0, _
							cbBytesAuthorization _
						)
						If this->CRequest.Headers(RequestHeaders.HeaderAuthorization) = NULL Then
							HttpRestFormCleanUp(this)
							DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTHEADER)
							Exit Sub
						End If

						lstrcpy( _
							this->CRequest.Headers(RequestHeaders.HeaderAuthorization), _
							@bufBasicUserPassword.szText(0) _
						)
					End Scope
				End If
			End If
		End Scope

		' Create Connection Header
		Scope
			Const CloseString = __TEXT("Close")
			Dim cbCloseString As Integer = (Len(CloseString) + 1) * SizeOf(TCHAR)
			this->CRequest.Headers(RequestHeaders.HeaderConnection) = HeapAlloc( _
				this->hHeap, _
				0, _
				cbCloseString _
			)
			If this->CRequest.Headers(RequestHeaders.HeaderConnection) = NULL Then
				HttpRestFormCleanUp(this)
				DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTHEADER)
				Exit Sub
			End If

			lstrcpy( _
				this->CRequest.Headers(RequestHeaders.HeaderConnection), _
				@CloseString _
			)
		End Scope

		' Create Content-Length Header later

		' Create Content-Type Header
		Scope
			Dim bufContentType As FileNameBuffer = Any
			Dim ContentTypeLength As Long = GetDlgItemText( _
				hWin, _
				IDC_EDT_TYPE, _
				@bufContentType.szText(0), _
				MAX_PATH _
			)

			Dim cbBytesContentType As Integer = (ContentTypeLength + 1) * SizeOf(TCHAR)
			this->CRequest.Headers(RequestHeaders.HeaderContentType) = HeapAlloc( _
				this->hHeap, _
				0, _
				cbBytesContentType _
			)
			If this->CRequest.Headers(RequestHeaders.HeaderContentType) = NULL Then
				HttpRestFormCleanUp(this)
				DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTHEADER)
				Exit Sub
			End If

			lstrcpy( _
				this->CRequest.Headers(RequestHeaders.HeaderContentType), _
				@bufContentType.szText(0) _
			)
		End Scope

		' Create Expect Header
		Scope
			Const Expect = __TEXT("100-continue")
			Dim cbBytesExpect As Integer = (Len(Expect) + 1) * SizeOf(TCHAR)
			this->CRequest.Headers(RequestHeaders.HeaderExpect) = HeapAlloc( _
				this->hHeap, _
				0, _
				cbBytesExpect _
			)
			If this->CRequest.Headers(RequestHeaders.HeaderExpect) = NULL Then
				HttpRestFormCleanUp(this)
				DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTHEADER)
				Exit Sub
			End If

			lstrcpy( _
				this->CRequest.Headers(RequestHeaders.HeaderExpect), _
				@Expect _
			)
		End Scope

		' Create User-Agent Header
		Scope
			Const UserAgent = __TEXT("HttpRestClient")
			Dim cbBytesUserAgent As Integer = (Len(UserAgent) + 1) * SizeOf(TCHAR)
			this->CRequest.Headers(RequestHeaders.HeaderUserAgent) = HeapAlloc( _
				this->hHeap, _
				0, _
				cbBytesUserAgent _
			)
			If this->CRequest.Headers(RequestHeaders.HeaderUserAgent) = NULL Then
				HttpRestFormCleanUp(this)
				DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTHEADER)
				Exit Sub
			End If

			lstrcpy( _
				this->CRequest.Headers(RequestHeaders.HeaderUserAgent), _
				@UserAgent _
			)
		End Scope

		' Create Host Header
		Scope
			Dim bufServerPortName As FileNameBuffer = Any
			Dim ServerPortNameLength As Long = GetDlgItemText( _
				hWin, _
				IDC_EDT_SERVER, _
				@bufServerPortName.szText(0), _
				MAX_PATH _
			)

			Dim cbBytesHeaderHost As Integer = (ServerPortNameLength + 1) * SizeOf(TCHAR)
			this->CRequest.Headers(RequestHeaders.HeaderHost) = HeapAlloc( _
				this->hHeap, _
				0, _
				cbBytesHeaderHost _
			)
			If this->CRequest.Headers(RequestHeaders.HeaderHost) = NULL Then
				HttpRestFormCleanUp(this)
				DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTHEADER)
				Exit Sub
			End If

			lstrcpy( _
				this->CRequest.Headers(RequestHeaders.HeaderHost), _
				@bufServerPortName.szText(0) _
			)

			Dim PortNumber As Integer = SplitServerPort( _
				@bufServerPortName.szText(0), _
				ServerPortNameLength _
			)

			this->CRequest.ServerPort = PortNumber
			this->CRequest.ServerAddress = gethostbyname(@bufServerPortName.szText(0))
		End Scope

		If this->CRequest.ServerAddress = NULL Then
			Dim dwError As Long = WSAGetLastError()
			HttpRestFormCleanUp(this)
			DisplayError(this->hInst, hWin, dwError, IDS_GETHOSTBYNAME)
			Exit Sub
		End If

	End Scope

	' Open File
	Scope
		If this->FileHandle = INVALID_HANDLE_VALUE Then
			Dim bufFileName As FileNameBuffer = Any
			Dim FileNameLength As UINT = GetDlgItemText( _
				hWin, _
				IDC_EDT_FILE, _
				@bufFileName.szText(0), _
				MAX_PATH _
			)

			If FileNameLength Then
				this->FileHandle = CreateFile( _
					@bufFileName.szText(0), _
					GENERIC_READ, _
					FILE_SHARE_READ, _
					NULL, _
					OPEN_EXISTING, _
					FILE_ATTRIBUTE_NORMAL, _
					NULL _
				)

				If this->FileHandle = INVALID_HANDLE_VALUE Then
					Dim dwError As DWORD = GetLastError()
					HttpRestFormCleanUp(this)
					DisplayError(this->hInst, hWin, dwError, IDS_CANNOTOPENFILE)
					Exit Sub
				End If

			End If
		End If

		If this->FileHandle <> INVALID_HANDLE_VALUE Then
			this->liFileSize.HighPart = 0
			this->liFileSize.LowPart = GetFileSize(this->FileHandle, @this->liFileSize.HighPart)

			If this->liFileSize.LowPart = INVALID_FILE_SIZE Then
				Dim dwError As DWORD = GetLastError()
				If dwError <> NO_ERROR Then
					HttpRestFormCleanUp(this)
					DisplayError(this->hInst, hWin, dwError, IDS_CANNOTGETFILESIZE)
					Exit Sub
				End If
			End If
		End If
	End Scope

	' Create File Mapping Object
	Scope
		If this->liFileSize.QuadPart Then

			Scope
				Const FormatString = __TEXT(!"%ld")

				Dim buf As ErrorBuffer = Any
				Dim Length As Long = wsprintf( _
					@buf.szText(0), _
					@FormatString, _
					this->liFileSize.QuadPart _
				)

				Dim cbContentLength As Integer = (Length + 1) * SizeOf(TCHAR)
				this->CRequest.Headers(RequestHeaders.HeaderContentLength) = HeapAlloc( _
					this->hHeap, _
					0, _
					cbContentLength _
				)
				If this->CRequest.Headers(RequestHeaders.HeaderContentLength) = NULL Then
					HttpRestFormCleanUp(this)
					DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTHEADER)
					Exit Sub
				End If

				lstrcpy( _
					this->CRequest.Headers(RequestHeaders.HeaderContentLength), _
					@buf.szText(0) _
				)
			End Scope

			this->MapFileHandle = CreateFileMapping( _
				this->FileHandle, _
				NULL, _
				PAGE_READONLY, _
				0, 0, _
				NULL _
			)
			If this->MapFileHandle = NULL Then
				Dim dwError As DWORD = GetLastError()
				HttpRestFormCleanUp(this)
				DisplayError(this->hInst, hWin, dwError, IDS_FILEMAPPING)
				Exit Sub
			End If
		End If
	End Scope

	' Create Socket
	Scope
		this->ClientSocket = socket_(AF_INET, SOCK_STREAM, IPPROTO_TCP)
		If this->ClientSocket = INVALID_SOCKET Then
			Dim dwError As Long = WSAGetLastError()
			HttpRestFormCleanUp(this)
			DisplayError(this->hInst, hWin, dwError, IDS_SOCKET)
			Exit Sub
		End If
	End Scope

	' Create Headers
	Scope
		HttpRestFormToString(this)

		If this->CRequest.AllHeaders = NULL Then
			HttpRestFormCleanUp(this)
			DisplayError(this->hInst, hWin, ERROR_NOT_ENOUGH_MEMORY, IDS_REQUESTHEADERS)
			Exit Sub
		End If
	End Scope

	' Display Progress Dialog
	Scope
		Dim DialogBoxParamResult As INT_PTR = DialogBoxParam( _
			this->hInst, _
			MAKEINTRESOURCE(IDD_DLG_PROGRESS), _
			hWin, _
			@ProgressDialogProc, _
			Cast(LPARAM, this) _
		)

		Select Case DialogBoxParamResult

			Case IDOK
				' Display response headers + body
				SetDlgItemText( _
					hWin, _
					IDC_EDT_RESPONSE, _
					this->pResponseMem _
				)

				' Cleanup
				HeapFree( _
					this->hHeap, _
					0, _
					this->pResponseMem _
				)
				this->ResponseLength = 0
				this->pResponseMem = NULL

				Dim pMem As HttpRestFormSettings Ptr = HeapAlloc( _
					this->hHeap, _
					0, _
					SizeOf(HttpRestFormSettings) _
				)

				If pMem Then
					Dim vec As SettingsVector = Any
					FillSettingsVector(@vec, pMem)

					For i As Integer = LBound(vec.Vec) To UBound(vec.Vec)
						vec.Vec(i).ValueLength = GetDlgItemText( _
							hWin, _
							vec.Vec(i).ControlId, _
							vec.Vec(i).Value, _
							MAX_PATH _
						)
					Next

					If this->IsTemporaryFile = FileType.Temporary Then
						vec.Vec(3).Value = NULL
						vec.Vec(3).ValueLength = 0
					End If

					' Save settings
					SaveSettings(@vec)

					HeapFree( _
						this->hHeap, _
						0, _
						pMem _
					)
				End If

			Case IDCANCEL
				' Cancel send receive data

			Case Else
				Dim dwError As DWORD = GetLastError()
				DisplayError(this->hInst, hWin, dwError, IDS_DIALOGPROGRESS)

		End Select

		If this->IsTemporaryFile = FileType.Temporary Then
			SetDlgItemText( _
				hWin, _
				IDC_EDT_FILE, _
				NULL _
			)
		End If

		this->IsTemporaryFile = FileType.DiskFile

	End Scope

End Sub

Private Function OpenFileNameShowDialog( _
		ByVal hInst As HINSTANCE, _
		ByVal hOwner As HWND, _
		ByVal pbuf As TCHAR Ptr, _
		ByVal pFileOffset As Integer Ptr, _
		ByVal pFileExtension As Integer Ptr _
	) As Boolean

	Dim FileFilter As ResourceStringBuffer = Any
	LoadString( _
		hInst, _
		IDS_FILTER, _
		@FileFilter.szText(0), _
		RESOURCE_STRING_BUFFER_CAPACITY _
	)

	Dim Caption As ResourceStringBuffer = Any
	LoadString( _
		hInst, _
		IDS_SELECTFILE, _
		@Caption.szText(0), _
		RESOURCE_STRING_BUFFER_CAPACITY _
	)

	pbuf[0] = 0

#if WINVER >= &h0500
	Dim dwSize As DWORD = SizeOf(OPENFILENAME)
#else
	' HACK for win95
	Dim dwSize As DWORD = OPENFILENAME_SIZE_VERSION_400
#endif

	Dim fn As OPENFILENAME = Any
	ZeroMemory(@fn, dwSize)

	fn.lStructSize = dwSize
	fn.hwndOwner = hOwner
	' fn.hInstance = NULL
	fn.lpstrFilter = @FileFilter.szText(0)
	' fn.lpstrCustomFilter = NULL
	' fn.nMaxCustFilter = 0
	fn.nFilterIndex = 1
	fn.lpstrFile = pbuf
	fn.nMaxFile = MAX_PATH
	' fn.lpstrFileTitle = NULL
	' fn.nMaxFileTitle = 0
	' fn.lpstrInitialDir = NULL
	fn.lpstrTitle = @Caption.szText(0)
	fn.Flags = OFN_FILEMUSTEXIST Or OFN_PATHMUSTEXIST
	' fn.nFileOffset = 0
	' fn.nFileExtension = 0
	' fn.lpstrDefExt = NULL
	' fn.lCustData = 0
	' fn.lpfnHook = NULL
	' fn.lpTemplateName = NULL
	' fn.lpEditInfo = NULL
	' fn.lpstrPrompt = NULL
	' fn.pvReserved = NULL
	' fn.dwReserved = 0
	' fn.FlagsEx = 0

	Dim resGetFile As BOOL = GetOpenFileName(@fn)

	If resGetFile = 0 Then
		Dim dwError As DWORD = CommDlgExtendedError()
		If dwError Then
			' DisplayError(this->hInst, hWin, dwError, IDS_OPENFILENAME)
			Return False
		End If
	End If

	*pFileOffset = CInt(fn.nFileOffset)
	*pFileExtension = CInt(fn.nFileExtension)

	If pbuf[0] = 0 Then
		Return False
	End If

	Return True

End Function

Private Sub BrowseButton_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	Dim buf As FileNameBuffer = Any
	Dim nFileOffset As Integer = Any
	Dim nFileExtension As Integer = Any
	Dim resOpen As Boolean = OpenFileNameShowDialog( _
		this->hInst, _
		hWin, _
		@buf.szText(0), _
		@nFileOffset, _
		@nFileExtension _
	)

	If resOpen = False Then
		Exit Sub
	End If

	If nFileOffset Then
		Dim IndexPrev As Integer = nFileOffset - 1
		Dim OldValue As Integer = buf.szText(IndexPrev)
		buf.szText(IndexPrev) = Asc("/")

		SetDlgItemText( _
			hWin, _
			IDC_EDT_RESOURCE, _
			@buf.szText(IndexPrev) _
		)

		buf.szText(IndexPrev) = OldValue
	End If

	SetDlgItemText( _
		hWin, _
		IDC_EDT_FILE, _
		@buf.szText(0) _
	)

	this->IsTemporaryFile = FileType.DiskFile

	If nFileExtension Then
		Dim ExtensionWithDotOffset As Integer = nFileExtension - 1
		Dim pExt As TCHAR Ptr = @buf.szText(ExtensionWithDotOffset)

		Dim bufContentType As FileNameBuffer = Any
		Dim hrContentType As HRESULT = GetContentTypeOfFileExtension( _
			@bufContentType.szText(0), _
			pExt, _
			MAX_PATH _
		)

		If SUCCEEDED(hrContentType) Then
			SetDlgItemText( _
				hWin, _
				IDC_EDT_TYPE, _
				@bufContentType.szText(0) _
			)
		End If
	End If

End Sub

Private Function FillBitmapFileHeader( _
		ByVal pbmfh As BITMAPFILEHEADER Ptr, _
		ByVal pbih As BITMAPINFOHEADER Ptr _
	)As DWORD

	' Convert to 24 bpp

	' pbih->biBitCount = 24
	Dim dwBPP As DWORD = pbih->biBitCount
	Dim dwWidth As Long = pbih->biWidth
	Dim dwHeight As Long = Abs(pbih->biHeight)
	Dim dwNumColors As DWORD = pbih->biClrUsed

	Const BitmapMagic = &h04D42

	Dim dwOffset As DWORD = SizeOf(BITMAPFILEHEADER) + SizeOf(BITMAPINFOHEADER) + (dwNumColors * SizeOf(RGBQUAD))

	' Magic Formula
	Dim stride As DWORD = (((dwWidth * dwBPP) + 31) And (Not 31)) Shr 3
	Dim dwLength As DWORD = dwHeight * stride
	Dim biSizeImage As DWORD = dwLength + dwOffset

	pbmfh->bfType = BitmapMagic
	pbmfh->bfSize = biSizeImage
	pbmfh->bfReserved1 = 0
	pbmfh->bfReserved2 = 0
	pbmfh->bfOffBits = dwOffset

	If pbih->biSizeImage = 0 Then
		pbih->biSizeImage = dwLength
	End If

	Return dwLength

End Function

Private Function BitmapFormatAvailable()As Boolean

	Dim resAvailable1 As BOOL = IsClipboardFormatAvailable(CF_DIB)
	If resAvailable1 = 0 Then
		Return False
	End If

	Dim resAvailable2 As BOOL = IsClipboardFormatAvailable(CF_BITMAP)
	If resAvailable2 = 0 Then
		Return False
	End If

	Return True

End Function

Private Function FillTemporaryFileName( _
		ByVal pFileName As TCHAR Ptr _
	)As HRESULT

	Const TempPathPrefix = __TEXT("HttpRestClient")

	Dim TempDir As FileNameBuffer = Any
	Dim resGetTempPath As DWORD = GetTempPath( _
		MAX_PATH, _
		@TempDir.szText(0) _
	)
	If resGetTempPath = 0 Then
		Dim dwError As DWORD = GetLastError()
		Return HRESULT_FROM_WIN32(dwError)
	End If

	Dim resGetTempFileName As UINT = GetTempFileName( _
		@TempDir.szText(0), _
		@TempPathPrefix, _
		0, _
		pFileName _
	)
	If resGetTempFileName = 0 Then
		Dim dwError As DWORD = GetLastError()
		Return HRESULT_FROM_WIN32(dwError)
	End If

	Return S_OK

End Function

Private Sub PasteButton_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	Dim resFormatAvailable As Boolean = BitmapFormatAvailable()
	If resFormatAvailable = False Then
		Exit Sub
	End If

	Dim resOpenClip As BOOL = OpenClipboard(hWin)
	If resOpenClip = 0 Then
		Exit Sub
	End If

	Dim pBinfo As BITMAPINFO Ptr = GetClipboardData(CF_DIB)
	Dim hBmp As HBITMAP = GetClipboardData(CF_BITMAP)

	If pBinfo AndAlso hBmp Then
		Dim hDCWindow As HDC = GetDC(hWin)

		If hDCWindow Then

			' We need two Memory DC to copy from it
			' to prevent stretch image if DPI enabled

			Dim hdcSource As HDC = CreateCompatibleDC(hDCWindow)

			If hdcSource Then

				Dim OldBmp As HBITMAP = SelectObject(hdcSource, hBmp)

				Dim hdcDestination As HDC = CreateCompatibleDC(hDCWindow)

				If hdcDestination Then

					Dim dwWidth As Long = pBinfo->bmiHeader.biWidth
					Dim dwHeight As Long = pBinfo->bmiHeader.biHeight

					Dim bih As BITMAPINFO = Any
					' Convert to 24 bpp
					With bih.bmiHeader
						.biSize          = sizeof(BITMAPINFOHEADER)
						.biWidth         = dwWidth
						.biHeight        = dwHeight
						.biPlanes        = 1
						.biBitCount      = 24
						.biCompression   = BI_RGB
						.biSizeImage     = 0
						.biXPelsPerMeter = 0
						.biYPelsPerMeter = 0
						.biClrUsed       = 0
						.biClrImportant  = 0
					End With

					Dim bmfh As BITMAPFILEHEADER = Any
					Dim dwLength As DWORD = FillBitmapFileHeader(@bmfh, @bih.bmiHeader)

					Dim pBits As LPVOID = Any
					Dim hNewBitmap As HBITMAP = CreateDIBSection(hdcDestination, @bih, DIB_RGB_COLORS, @pBits, NULL, 0)

					If hNewBitmap Then

						Dim OldMemBmpDestination As HBITMAP = SelectObject(hdcDestination, hNewBitmap)

						Dim resBlt As BOOL = BitBlt(hdcDestination, 0,0, dwWidth, Abs(dwHeight), hdcSource, 0,0, SRCCOPY)

						If resBlt Then

							Dim TempFileName As FileNameBuffer = Any
							Dim hrGetTempFileName As HRESULT = FillTemporaryFileName( _
								@TempFileName.szText(0) _
							)

							If SUCCEEDED(hrGetTempFileName) Then

								Dim hBitmapFile As HANDLE = CreateFile( _
									@TempFileName.szText(0), _
									GENERIC_READ Or GENERIC_WRITE, _
									0, _
									NULL, _
									CREATE_ALWAYS, _
									FILE_ATTRIBUTE_TEMPORARY Or FILE_FLAG_DELETE_ON_CLOSE, _
									NULL _
								)

								If hBitmapFile <> INVALID_HANDLE_VALUE Then

									SetDlgItemText( _
										hWin, _
										IDC_EDT_FILE, _
										@TempFileName.szText(0) _
									)

									Const MimeBitmap = __TEXT("image/bmp")
									SetDlgItemText( _
										hWin, _
										IDC_EDT_TYPE, _
										@MimeBitmap _
									)

									this->IsTemporaryFile = FileType.Temporary

									' Write File Header
									Scope
										Dim dwWritedBytes As DWORD = Any
										WriteFile(hBitmapFile, @bmfh, SizeOf(BITMAPFILEHEADER), @dwWritedBytes, NULL)
									End Scope

									' Write Info Header
									Scope
										Dim dwWritedBytes As DWORD = Any
										WriteFile(hBitmapFile, @bih.bmiHeader, SizeOf(BITMAPINFOHEADER), @dwWritedBytes, NULL)
									End Scope

									' Write Color Table
									Scope
										' If pBinfo->bmiHeader.biClrUsed Then
										' 	WriteFile(hBitmapFile, pBinfo->bmiHeader, SizeOf(BITMAPINFOHEADER), @dwWritedBytes, NULL)
										' End If
									End Scope

									' Write Bytes
									Scope
										Dim dwWritedBytes As DWORD = Any
										WriteFile(hBitmapFile, pBits, dwLength, @dwWritedBytes, NULL)
									End Scope

									SetFilePointer( _
										hBitmapFile, _
										0, _
										NULL, _
										FILE_BEGIN _
									)

									' CloseHandle(hBitmapFile)
									this->FileHandle = hBitmapFile
								End If
							End If
						End If

						SelectObject(hdcDestination, OldMemBmpDestination)
						DeleteObject(hNewBitmap)
					End If

					DeleteDC(hdcDestination)
				End If

				SelectObject(hdcSource, OldBmp)
				DeleteDC(hdcSource)
			End If

			ReleaseDC(hWin, hDCWindow)
		End If
	End If

	CloseClipboard()

End Sub

Private Sub CopyButton_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	Dim bufServer As FileNameBuffer = Any
	Dim ServerLength As Long = GetDlgItemText( _
		hWin, _
		IDC_EDT_SERVER, _
		@bufServer.szText(0), _
		MAX_PATH _
	)

	Dim bufResource As FileNameBuffer = Any
	Dim ResourceLength As Long = GetDlgItemText( _
		hWin, _
		IDC_EDT_RESOURCE, _
		@bufResource.szText(0), _
		MAX_PATH _
	)

	If ServerLength AndAlso ResourceLength Then

		Const FormatString = __TEXT(!"http://%s%s")

		Dim Url As FileNameBuffer = Any

		Dim Length As Long = wsprintf( _
			@Url.szText(0), _
			@FormatString, _
			@bufServer.szText(0), _
			@bufResource.szText(0) _
		)

		Dim resOpen As BOOL = OpenClipboard(hWin)

		If resOpen Then

			' Need empty clipboard
			EmptyClipboard()

			Dim cbBytes As Integer = (Length + 1) * SizeOf(TCHAR)
			Dim hglbCopy As HGLOBAL = GlobalAlloc( _
				GMEM_MOVEABLE, _
				cbBytes _
			)

			If hglbCopy = NULL Then
				CloseClipboard()
				Exit Sub
			End If

			Dim lptstrCopy As LPTSTR = GlobalLock(hglbCopy)

			If lptstrCopy = NULL Then
				GlobalFree(hglbCopy)
				CloseClipboard()
				Exit Sub
			End If

			MoveMemory(lptstrCopy, @Url.szText(0), cbBytes)

			GlobalUnlock(hglbCopy)

			SetClipboardData(CF_TEXT, hglbCopy)

			CloseClipboard()
		End If
	End If

End Sub

Private Sub DialogMain_OnUnload( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	EndDialog(hWin, IDCANCEL)

End Sub

Private Sub DialogMain_OnDragDropFile( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND, _
		ByVal drp As HDROP _
	)

	Const FileIndex = 0

	Dim FileName As FileNameBuffer = Any

	Dim resDrag As UINT = DragQueryFile( _
		drp, _
		FileIndex, _
		@FileName.szText(0), _
		MAX_PATH _
	)

	If resDrag Then
		SetDlgItemText( _
			hWin, _
			IDC_EDT_FILE, _
			@FileName.szText(0) _
		)

		Dim ExtensionWithDotOffset As Integer = -1

		For i As Integer = resDrag - 1 To 0 Step -1
			Dim Code As Integer = FileName.szText(i)
			If Code = Asc(".") Then
				ExtensionWithDotOffset = i
				Exit For
			End If
		Next

		If ExtensionWithDotOffset <> -1 Then
			Dim pExt As TCHAR Ptr = @FileName.szText(ExtensionWithDotOffset)

			Dim bufRegValue As FileNameBuffer = Any
			Dim hrContentType As HRESULT = GetContentTypeOfFileExtension( _
				@bufRegValue.szText(0), _
				pExt, _
				MAX_PATH _
			)

			If SUCCEEDED(hrContentType) Then
				SetDlgItemText( _
					hWin, _
					IDC_EDT_TYPE, _
					@bufRegValue.szText(0) _
				)
			End If
		End If
	End If

End Sub

Private Sub TxtServer_Changed( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)

	Dim hwndServer As HWND = GetDlgItem(hWin, IDC_EDT_SERVER)
	Dim hwndResource As HWND = GetDlgItem(hWin, IDC_EDT_RESOURCE)
	Dim hwndVerb As HWND = GetDlgItem(hWin, IDC_CBB_VERB)

	Dim LengthServer As Integer = GetWindowTextLength(hwndServer)
	Dim LengthResource As Integer = GetWindowTextLength(hwndResource)
	Dim LengthVerb As Integer = GetWindowTextLength(hwndVerb)

	If LengthServer Then
		If LengthResource Then
			If LengthVerb Then
				EnableDialogItem(hWin, IDOK)
				Exit Sub
			End If
		End If
	End If

	DisableDialogItem(hWin, IDOK)

End Sub

Private Function InputDataDialogProc( _
		ByVal hWin As HWND, _
		ByVal uMsg As UINT, _
		ByVal wParam As WPARAM, _
		ByVal lParam As LPARAM _
	)As INT_PTR

	Dim pParam As HttpRestForm Ptr = Any

	If uMsg = WM_INITDIALOG Then
		pParam = Cast(HttpRestForm Ptr, lParam)
		DialogMain_OnLoad(pParam, hWin)
		SetWindowLongPtr(hWin, GWLP_USERDATA, Cast(LONG_PTR, pParam))

		Return True
	End If

	pParam = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))

	Select Case uMsg

		Case WM_COMMAND

			Dim Reason As WORD = HIWORD(wParam)
			Dim ControlId As WORD = LOWORD(wParam)

			Select Case Reason

				Case BN_CLICKED
					' Menu or Button

					Select Case ControlId

						Case IDOK
							IDOK_OnClick(pParam, hWin)

						Case IDCANCEL
							IDCANCEL_OnClick(pParam, hWin)

						Case IDC_BTN_BROWSE
							BrowseButton_OnClick(pParam, hWin)

						Case IDC_BTN_PASTE
							PasteButton_OnClick(pParam, hWin)

						Case IDC_BTN_COPY
							CopyButton_OnClick(pParam, hWin)

						Case Else
							Return False

					End Select

				' Case 1
					' Accelerator
					' Return False

				Case EN_CHANGE

					Select Case ControlId

						Case IDC_EDT_SERVER
							TxtServer_Changed(pParam, hWin)

						Case IDC_EDT_RESOURCE
							TxtServer_Changed(pParam, hWin)

						Case Else
							Return False

					End Select

				Case CBN_EDITCHANGE

					Select Case ControlId

						Case IDC_CBB_VERB
							TxtServer_Changed(pParam, hWin)

						Case Else
							Return False

					End Select

				Case Else
					Return False

			End Select

		Case WM_CLOSE
			DialogMain_OnUnload(pParam, hWin)

		Case WM_DROPFILES
			Dim drp As HDROP = Cast(HDROP, wParam)
			DialogMain_OnDragDropFile(pParam, hWin, drp)
			DragFinish(drp)

		Case Else
			Return FALSE

	End Select

	Return TRUE

End Function

Private Function EnableVisualStyles()As HRESULT

	' Only for Win95
	InitCommonControls()

	' Dim icc As INITCOMMONCONTROLSEX = Any
	' icc.dwSize = SizeOf(INITCOMMONCONTROLSEX)
	' icc.dwICC = ICC_ANIMATE_CLASS Or _
	' 	ICC_BAR_CLASSES Or _
	' 	ICC_COOL_CLASSES Or _
	' 	ICC_DATE_CLASSES Or _
	' 	ICC_HOTKEY_CLASS Or _
	' 	ICC_INTERNET_CLASSES Or _
	' 	ICC_LINK_CLASS Or _
	' 	ICC_LISTVIEW_CLASSES Or _
	' 	ICC_NATIVEFNTCTL_CLASS Or _
	' 	ICC_PAGESCROLLER_CLASS Or _
	' 	ICC_PROGRESS_CLASS Or _
	' 	ICC_STANDARD_CLASSES Or _
	' 	ICC_TAB_CLASSES Or _
	' 	ICC_TREEVIEW_CLASSES Or _
	' 	ICC_UPDOWN_CLASS Or _
	' 	ICC_USEREX_CLASSES Or _
	' ICC_WIN95_CLASSES

	' Dim res As BOOL = InitCommonControlsEx(@icc)
	' If res = 0 Then
	' 	Dim dwError As DWORD = GetLastError()
	' 	Return HRESULT_FROM_WIN32(dwError)
	' End If

	Return S_OK

End Function

Private Function tWinMain( _
		Byval hInst As HINSTANCE, _
		ByVal hPrevInstance As HINSTANCE, _
		ByVal lpCmdLine As LPTSTR, _
		ByVal iCmdShow As Long _
	)As Integer

	Dim hrVisualStyles As Integer = EnableVisualStyles()

	If FAILED(hrVisualStyles) Then
		Return 1
	End If

	Dim hWin As HWND = GetDesktopWindow()

	Dim ProcessHeap As HANDLE = GetProcessHeap()

	' We need allocate memory for HttpRestForm
	' Beekause when main thread is terminated
	' Thread stack not exists
	Dim param As HttpRestForm Ptr = HeapAlloc( _
		ProcessHeap, _
		0, _
		SizeOf(HttpRestForm) _
	)

	If param Then

		Dim hrInitNetwork As HRESULT = NetworkStartUp()

		If FAILED(hrInitNetwork) Then
			DisplayError(hInst, hWin, hrInitNetwork, IDS_NETWORKINIT)
		Else

			param->hInst = hInst
			param->FileHandle = INVALID_HANDLE_VALUE
			param->MapFileHandle = NULL
			param->ClientSocket = INVALID_SOCKET
			param->liFileSize.HighPart = 0
			param->liFileSize.LowPart = 0
			param->IsTemporaryFile = FileType.DiskFile
			param->hHeap = ProcessHeap
			param->hEvent = CreateEvent(NULL, TRUE, FALSE, NULL)

			ZeroMemory(@param->CRequest, SizeOf(ClientRequest))

			Dim ThreadId As DWORD = Any
			Dim hThread As HANDLE = CreateThread( _
				NULL, _
				0, _
				@WorkerThread, _
				param, _
				0, _
				@ThreadId _
			)

			If hThread = NULL Then
				Dim dwError As DWORD = GetLastError()
				DisplayError(hInst, hWin, dwError, IDS_THREAD)
			Else

				CloseHandle(hThread)

				Dim DialogBoxParamResult As INT_PTR = DialogBoxParam( _
					hInst, _
					MAKEINTRESOURCE(IDD_DLG_TASKS), _
					hWin, _
					@InputDataDialogProc, _
					Cast(LPARAM, param) _
				)

				If DialogBoxParamResult = -1 Then
					Dim dwError As DWORD = GetLastError()
					DisplayError(hInst, hWin, dwError, IDS_DIALOGMAIN)
				End If

			End If

			CloseHandle(param->hEvent)
			NetworkCleanUp()
		End If

	End If

	Return 0

End Function

Dim hInst As HMODULE = GetModuleHandle(NULL)

' The program does not process command line parameters
Dim Arguments As LPTSTR = NULL
Dim RetCode As Integer = tWinMain( _
	hInst, _
	NULL, _
	Arguments, _
	SW_SHOW _
)

End(RetCode)
