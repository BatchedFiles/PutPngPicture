#include once "WinMain.bi"
#include once "win\commctrl.bi"
#include once "win\commdlg.bi"
#include once "win\windowsx.bi"
#include once "win\mswsock.bi"
#include once "Resources.RH"

Const NETEVENT_NOTICE = WM_USER + 2

Enum NetEventKind
	Connect
	SendHeaders
	SendBody
	ReadRequest
	Done
End Enum

' HACK for Win95
#ifdef RtlMoveMemory
#undef RtlMoveMemory
#endif
#ifdef RtlZeroMemory
#undef RtlZeroMemory
#endif
#ifdef MoveMemory
#undef MoveMemory
#endif
#ifdef ZeroMemory
#undef ZeroMemory
#endif

Declare Sub RtlMoveMemory Alias "RtlMoveMemory"(ByVal Destination As Any Ptr, ByVal Source As Any Ptr, ByVal Length As Integer)
Declare Sub RtlZeroMemory Alias "RtlZeroMemory"(ByVal Destination As Any Ptr, ByVal Length As Integer)
#define MoveMemory(d, s, l) RtlMoveMemory((d), (s), (l))
#define ZeroMemory(d, l) RtlZeroMemory((d), (l))

Enum RequestHeaders
	HeaderAccept
	HeaderAuthorization
	HeaderConnection
	HeaderContentLength
	HeaderContentType
	HeaderHost
	HeaderUserAgent
End Enum

Const RequestHeadersLength = 7

Type ClientRequest
	RequestLine As TCHAR Ptr
	ServerAddress As HOSTENT Ptr
	ServerPort As Integer
	AllHeaders As TCHAR Ptr
	AllHeadersLength As Integer
	Headers(RequestHeadersLength - 1) As TCHAR Ptr
End Type

Type HttpRestForm
	hInst As HINSTANCE
	FileHandle As HANDLE
	MapFileHandle As HANDLE
	ClientSocket As SOCKET
	liFileSize As LARGE_INTEGER
	hHeap As HANDLE
	hEvent As HANDLE
	hWin As HWND
	CRequest As ClientRequest
End Type

Type ResourceStringBuffer
	szText(255) As TCHAR
End Type

Type FileNameBuffer
	szText(MAX_PATH) As TCHAR
End Type

Type ErrorBuffer
	szText(255) As TCHAR
End Type

Private Function E0(v1 As UByte)As UByte
	Return v1 shr 2
End Function

Private Function E1(v1 As UByte, v2 As UByte)As UByte
	Return ((v1 And 3) shl 4) + (v2 shr 4)
End Function

Private Function E2(v2 As UByte, v3 As UByte)As UByte
	Return ((v2 And &H0F) shl 2) + (v3 shr 6)
End Function

Private Function E3(v3 As UByte)As UByte
	Return v3 And &H3F
End Function

Const B64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

Private Function Encode64( _
		ByVal sEncodedB As UByte Ptr, _
		ByVal BytesCount As Integer, _
		ByVal WithCrLf As Boolean, _
		ByVal sOut As TCHAR Ptr _
	)As Integer
	
	Const Base64LineWithoutCrLfLength As Integer = 19
	
	Dim ELM3 As Integer = BytesCount Mod 3
	Dim k As Integer = 0
	Dim j As Integer = 0
	
	For j = 0 To BytesCount - ELM3 - 1 Step 3
		' ��������� �� ����� ������, ���� �� ���������
		If WithCrLf Then
			If (j Mod Base64LineWithoutCrLfLength ) = 0 AndAlso j > 0 Then
				sOut[k + 0] = 13
				sOut[k + 1] = 10
				k += 2
			End If
		End If
		sOut[k + 0] = (@B64 + E0(sEncodedB[j + 0]))[0]
		sOut[k + 1] = (@B64 + E1(sEncodedB[j + 0], sEncodedB[j + 1]))[0]
		sOut[k + 2] = (@B64 + E2(sEncodedB[j + 1], sEncodedB[j + 2]))[0]
		sOut[k + 3] = (@B64 + E3(sEncodedB[j + 2]))[0]
		k += 4
	Next
	
	Select Case ELM3
		Case 1
			sOut[k + 0] = (@B64 + E0(sEncodedB[j + 0]))[0]
			sOut[k + 1] = (@B64 + E1(sEncodedB[j + 0], sEncodedB[j + 1]))[0]
			sOut[k + 2] = 61
			sOut[k + 3] = 61
			k += 4
		Case 2
			sOut[k + 0] = (@B64 + E0(sEncodedB[j + 0]))[0]
			sOut[k + 1] = (@B64 + E1(sEncodedB[j + 0], sEncodedB[j + 1]))[0]
			sOut[k + 2] = (@B64 + E2(sEncodedB[j + 1], sEncodedB[j + 2]))[0]
			sOut[k + 3] = 61
			k += 4
	End Select
	
	' Set terminate Nul Character
	sOut[k] = 0
	
	Return k
	
End Function

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
	End If
	
	If this->CRequest.AllHeaders Then
		HeapFree(this->hHeap, 0, this->CRequest.AllHeaders)
		this->CRequest.AllHeaders = NULL
	End If
	
	RestHeadersCleanup(this)
	
End Sub

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
	
	Const PageSize = 65536
	
	Dim Offset As LARGE_INTEGER = Any
	Offset.QuadPart = 0
	
	Do
		Dim Tail As LARGE_INTEGER = Any
		Tail.QuadPart = this->liFileSize.QuadPart - Offset.QuadPart
		
		Dim cbBytes As DWORD = min(PageSize, Tail.QuadPart)
		
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
		
		If cbBytes < PageSize Then
			Exit Do
		End If
		
		Offset.QuadPart = Offset.QuadPart + PageSize
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
						PostMessage( _
							this->hWin, _
							NETEVENT_NOTICE, _
							NetEventKind.Connect, _
							dwError _
						)
						Continue Do
					End If
				End Scope
				
				Dim resSend As Long = send( _
					this->ClientSocket, _
					this->CRequest.AllHeaders, _
					this->CRequest.AllHeadersLength, _
					0 _
				)
				If resSend = SOCKET_ERROR Then
					Dim dwError As Long = WSAGetLastError()
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
					NetEventKind.Done, _
					0 _
				)
				
			Case Else
				Exit Do
				
		End Select
		
	Loop
	
	Return 0
	
End Function

Private Sub DisplayError( _
		ByVal hWin As HWND, _
		ByVal dwErrorCode As DWORD, _
		ByVal Caption As LPCTSTR _
	)
	
	Const FormatString = __TEXT(!"Error code %d")
	
	Dim buf As ErrorBuffer = Any
	wsprintf( _
		@buf.szText(0), _
		@FormatString, _
		dwErrorCode _
	)
	
	MessageBox(hWin, @buf.szText(0), Caption, MB_OK Or MB_ICONERROR)
	
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

Private Function DivideServerPort( _
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
		ByVal nEvent As Long, _
		ByVal nError As Long _
	)
	
	Select Case nEvent
		
		Case NetEventKind.Connect
			If nError Then
				Const Caption = __TEXT("Connect to Server")
				DisplayError(hWin, nError, @Caption)
			End If
			
		Case NetEventKind.SendHeaders
			If nError Then
				Const Caption = __TEXT("Send Headers")
				DisplayError(hWin, nError, @Caption)
			End If
			
		Case NetEventKind.SendBody
			If nError Then
				Const Caption = __TEXT("Send Body")
				DisplayError(hWin, nError, @Caption)
			End If
			
		Case NetEventKind.ReadRequest
			If nError Then
				Const Caption = __TEXT("ReadRequest")
				DisplayError(hWin, nError, @Caption)
			End If
			
		Case NetEventKind.Done
			Const Caption = __TEXT("Done")
			DisplayError(hWin, nError, @Caption)
			
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
	
	' SetEvent(this->hEvent)
	this->hWin = hWin
	
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
	
	Select Case uMsg
		
		Case WM_INITDIALOG
			Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, lParam)
			DialogProgress_OnLoad(pParam, hWin)
			SetWindowLongPtr(hWin, GWLP_USERDATA, Cast(LONG_PTR, pParam))
			
		Case WM_COMMAND
			Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))
			
			Select Case LOWORD(wParam)
				
				Case IDCANCEL
					IDCANCEL2_OnClick(pParam, hWin)
					
			End Select
			
		Case NETEVENT_NOTICE
			Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))
			Dim nEvent As Long = wParam
			Dim nError As Long = CLng(lParam)
			Socket_OnWSANetEvent(pParam, hWin, nEvent, nError)
			
		Case WM_CLOSE
			Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))
			DialogProgress_OnUnload(pParam, hWin)
			
		Case Else
			Return FALSE
			
	End Select
	
	Return TRUE
	
End Function

Private Sub DialogMain_OnLoad( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	Dim hcn As HICON = LoadIcon(this->hInst, MAKEINTRESOURCE(IDI_MAIN))
	SendMessage(hWin, WM_SETICON, ICON_SMALL, Cast(LPARAM, hcn))
	SendMessage(hWin, WM_SETICON, ICON_BIG, Cast(LPARAM, hcn))

	this->hWin = hWin
	
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
				IDC_EDT_VERB, _
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
				Const Caption = __TEXT("Method and Path must be present")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
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
				Const Caption = __TEXT("Not enough memory")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
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
				Const Caption = __TEXT("Not enough memory")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
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
					IDC_EDT_USER, _
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
							False, _
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
							Const Caption = __TEXT("Not enough memory")
							HttpRestFormCleanUp(this)
							DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
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
				Const Caption = __TEXT("Not enough memory")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
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
				Const Caption = __TEXT("Not enough memory")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
				Exit Sub
			End If
			
			lstrcpy( _
				this->CRequest.Headers(RequestHeaders.HeaderContentType), _
				@bufContentType.szText(0) _
			)
		End Scope
		
		' Create User-Agent Header
		Scope
			Const UserAgent = __TEXT("RestClient")
			Dim cbBytesUserAgent As Integer = (Len(UserAgent) + 1) * SizeOf(TCHAR)
			this->CRequest.Headers(RequestHeaders.HeaderUserAgent) = HeapAlloc( _
				this->hHeap, _
				0, _
				cbBytesUserAgent _
			)
			If this->CRequest.Headers(RequestHeaders.HeaderUserAgent) = NULL Then
				Const Caption = __TEXT("Not enough memory")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
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
				Const Caption = __TEXT("Not enough memory")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
				Exit Sub
			End If
			
			lstrcpy( _
				this->CRequest.Headers(RequestHeaders.HeaderHost), _
				@bufServerPortName.szText(0) _
			)
			
			Dim PortNumber As Integer = DivideServerPort( _
				@bufServerPortName.szText(0), _
				ServerPortNameLength _
			)
			
			this->CRequest.ServerPort = PortNumber
			this->CRequest.ServerAddress = gethostbyname(@bufServerPortName.szText(0))
		End Scope
		
		If this->CRequest.ServerAddress = NULL Then
			Const Caption = __TEXT("Get Host by Name")
			Dim dwError As Long = WSAGetLastError()
			HttpRestFormCleanUp(this)
			DisplayError(hWin, dwError, @Caption)
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
			If FileNameLength = 0 Then
				Const Caption = __TEXT("File name must be present")
				DisplayError(hWin, 0, @Caption)
				Exit Sub
			End If
			
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
				Const Caption = __TEXT("CreateFile")
				Dim dwError As DWORD = GetLastError()
				HttpRestFormCleanUp(this)
				DisplayError(hWin, dwError, @Caption)
				Exit Sub
			End If
		End If
	End Scope
	
	' Create File Mapping Object
	Scope
		this->liFileSize.HighPart = 0
		this->liFileSize.LowPart = GetFileSize(this->FileHandle, @this->liFileSize.HighPart)
		Scope
			Dim dwError As DWORD = GetLastError()
			If this->liFileSize.LowPart = INVALID_FILE_SIZE Then
				If dwError <> NO_ERROR Then
					Const Caption = __TEXT("GetFileSize")
					HttpRestFormCleanUp(this)
					DisplayError(hWin, dwError, @Caption)
					Exit Sub
				End If
			End If
		End Scope
		
		If this->liFileSize.LowPart = 0 Then
			If this->liFileSize.HighPart = 0 Then
				Const Caption = __TEXT("FileSize must be greater then zero")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, 0, @Caption)
				Exit Sub
			End If
		End If
		
		Scope
			Const FormatString = __TEXT(!"%d")
			
			Dim buf As ErrorBuffer = Any
			Dim Length As Long = wsprintf( _
				@buf.szText(0), _
				@FormatString, _
				this->liFileSize.LowPart _
			)
			
			Dim cbContentLength As Integer = (Length + 1) * SizeOf(TCHAR)
			this->CRequest.Headers(RequestHeaders.HeaderContentLength) = HeapAlloc( _
				this->hHeap, _
				0, _
				cbContentLength _
			)
			If this->CRequest.Headers(RequestHeaders.HeaderContentLength) = NULL Then
				Const Caption = __TEXT("Not enough memory")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
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
			Const Caption = __TEXT("CreateFileMapping")
			Dim dwError As DWORD = GetLastError()
			HttpRestFormCleanUp(this)
			DisplayError(hWin, dwError, @Caption)
			Exit Sub
		End If
	End Scope
	
	' Create Socket
	Scope
		this->ClientSocket = socket_(AF_INET, SOCK_STREAM, IPPROTO_TCP)
		If this->ClientSocket = INVALID_SOCKET Then
			Const Caption = __TEXT("Create Socket")
			Dim dwError As Long = WSAGetLastError()
			HttpRestFormCleanUp(this)
			DisplayError(hWin, dwError, @Caption)
			Exit Sub
		End If
	End Scope
	
	' Create Headers
	Scope
		HttpRestFormToString(this)
		
		If this->CRequest.AllHeaders = NULL Then
			Const Caption = __TEXT("Out of Memory")
			HttpRestFormCleanUp(this)
			DisplayError(hWin, ERROR_NOT_ENOUGH_MEMORY, @Caption)
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
				
			Case IDCANCEL
				' Cancel send receive data
				
			Case Else
				Const WinText2 = __TEXT("DialogBoxParam")
				Const WinCaption2 = __TEXT("Error")
				Dim dwError As DWORD = GetLastError()
				DisplayError(hWin, dwError, @WinText2)
			
		End Select
	End Scope
	
End Sub

Private Sub BrowseButton_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	Const FileFilter = __TEXT(!"All files (*)\0*\0\0")
	Const Caption = __TEXT("Select File to send")
	
	Dim buf As FileNameBuffer = Any
	buf.szText(0) = 0
	
	Dim fn As OPENFILENAME = Any
	With fn
		' HACK for win95
		.lStructSize = SizeOf(OPENFILENAME) - SizeOf(Any Ptr) - SizeOf(DWORD) - SizeOf(DWORD)
		.hwndOwner = hWin
		.hInstance = this->hInst
		.lpstrFilter = @FileFilter
		.lpstrCustomFilter = NULL
		.nMaxCustFilter = 0
		.nFilterIndex = 1
		.lpstrFile = @buf.szText(0)
		.nMaxFile = MAX_PATH
		.lpstrFileTitle = NULL
		.nMaxFileTitle = 0
		.lpstrInitialDir = NULL
		.lpstrTitle = @Caption
		.Flags = 0
		.nFileOffset = 0
		.nFileExtension = 0
		.lpstrDefExt = NULL
		.lCustData = 0
		.lpfnHook = NULL
		.lpTemplateName = NULL
		' .lpEditInfo = NULL
		' .lpstrPrompt = NULL
		.pvReserved = NULL
		.dwReserved = 0
		.FlagsEx = 0
	End With
	
	Dim resGetFile As BOOL = GetOpenFileName(@fn)
	
	If resGetFile = 0 Then
		Const WinText2 = __TEXT("GetOpenFileName")
		
		Dim dwError As DWORD = CommDlgExtendedError()
		If dwError Then
			DisplayError(hWin, dwError, @WinText2)
			
			Exit Sub
		End If
	End If
	
	SetDlgItemText( _
		hWin, _
		IDC_EDT_FILE, _
		@buf.szText(0) _
	)
	
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
	
	Const TempPathPrefix = __TEXT("HttpRest")
	
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
		Dim dwBPP As DWORD = pBinfo->bmiHeader.biBitCount
		
		If dwBPP = 24 OrElse dwBPP = 32 Then
			
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
						If dwBPP = 32 Then
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
						Else
							MoveMemory(@bih.bmiHeader, @pBinfo->bmiHeader, SizeOf(BITMAPINFOHEADER))
						End If
						
						Dim bmfh As BITMAPFILEHEADER = Any
						Dim dwLength As DWORD = FillBitmapFileHeader(@bmfh, @bih.bmiHeader)
						
						Dim pBits As LPVOID = Any
						Dim hNewBitmap As HBITMAP = CreateDIBSection(hdcSource, @bih, DIB_RGB_COLORS, @pBits, NULL, 0)
						
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
	End If
	
	CloseClipboard()
	
End Sub

Private Sub DialogMain_OnUnload( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	EndDialog(hWin, IDCANCEL)
	
End Sub

Private Function InputDataDialogProc( _
		ByVal hWin As HWND, _
		ByVal uMsg As UINT, _
		ByVal wParam As WPARAM, _
		ByVal lParam As LPARAM _
	)As INT_PTR
	
	Select Case uMsg
		
		Case WM_INITDIALOG
			Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, lParam)
			DialogMain_OnLoad(pParam, hWin)
			SetWindowLongPtr(hWin, GWLP_USERDATA, Cast(LONG_PTR, pParam))
			
		Case WM_COMMAND
			Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))
			
			Select Case LOWORD(wParam)
				
				Case IDOK
					IDOK_OnClick(pParam, hWin)
					
				Case IDCANCEL
					IDCANCEL_OnClick(pParam, hWin)
					
				Case IDC_BTN_BROWSE
					BrowseButton_OnClick(pParam, hWin)
					
				Case IDC_BTN_PASTE
					PasteButton_OnClick(pParam, hWin)
					
			End Select
			
		Case WM_CLOSE
			Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))
			DialogMain_OnUnload(pParam, hWin)
			
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
	
	If SUCCEEDED(hrVisualStyles) Then
		
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
				Const WinText2 = __TEXT("Network initialize")
				Const WinCaption2 = __TEXT("Error")
				DisplayError(hWin, hrInitNetwork, @WinText2)
			Else
				
				param->hInst = hInst
				param->FileHandle = INVALID_HANDLE_VALUE
				param->MapFileHandle = NULL
				param->ClientSocket = INVALID_SOCKET
				param->liFileSize.HighPart = 0
				param->liFileSize.LowPart = 0
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
					Const Caption = __TEXT("Create Thread")
					Dim dwError As DWORD = GetLastError()
					DisplayError(hWin, dwError, @Caption)
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
						Const WinText2 = __TEXT("DialogBoxParam")
						Const WinCaption2 = __TEXT("Error")
						Dim dwError As DWORD = GetLastError()
						DisplayError(hWin, dwError, @WinText2)
					End If
					
				End If
				
				CloseHandle(param->hEvent)
				NetworkCleanUp()
			End If
			
		End If
	End If
	
	Return 1
	
End Function

#ifndef WITHOUT_RUNTIME
Private Function EntryPoint()As Integer
#else
Public Function EntryPoint Alias "EntryPoint"()As Integer
#endif
	
	Dim hInst As HMODULE = GetModuleHandle(NULL)
	
	' The program does not process command line parameters
	Dim Arguments As LPTSTR = NULL
	Dim RetCode As Integer = tWinMain( _
		hInst, _
		NULL, _
		Arguments, _
		SW_SHOW _
	)
	
	#ifdef WITHOUT_RUNTIME
		ExitProcess(RetCode)
	#endif
	
	Return RetCode
	
End Function

#ifndef WITHOUT_RUNTIME
Dim RetCode As Long = CLng(EntryPoint())
End(RetCode)
#endif
