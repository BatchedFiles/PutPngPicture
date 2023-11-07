#include once "WinMain.bi"
#include once "win\commctrl.bi"
#include once "win\commdlg.bi"
#include once "win\windowsx.bi"
#include once "win\mswsock.bi"
#include once "Resources.RH"

Const WSA_NETEVENT = WM_USER + 2

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
	Padding As Integer
	Headers(RequestHeadersLength - 1) As TCHAR Ptr
End Type

Type HttpRestForm
	hInst As HINSTANCE
	FileHandle As HANDLE
	MapFileHandle As HANDLE
	ClientSocket As SOCKET
	liFileSize As LARGE_INTEGER
	CRequest As ClientRequest
	hHeap As HANDLE
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

Type ServerWithPort
	ServerName As TCHAR Ptr
	Port As Integer
End Type

Private Sub HttpRestFormCleanUp( _
		ByVal this As HttpRestForm Ptr _
	)
	
	If this->ClientSocket <> INVALID_SOCKET Then
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

Private Sub DivideServerPort( _
		ByVal psp As ServerWithPort Ptr, _
		ByVal pszText As TCHAR Ptr, _
		ByVal Length As Integer _
	)
	
	psp->ServerName = pszText
	
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
	
	If pszPort Then
		Dim PortLength As Long = lstrlen(pszPort)
		If PortLength Then
			psp->Port = StringToInteger(pszPort)
		Else
			psp->Port = 80
		End If
	Else
		psp->Port = 80
	End If
	
End Sub

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

Private Sub DialogMain_OnLoad( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	Dim hrInitNetwork As HRESULT = NetworkStartUp()
	If FAILED(hrInitNetwork) Then
		Const WinText2 = __TEXT("Network initialize")
		Const WinCaption2 = __TEXT("Error")
		
		DisplayError(hWin, hrInitNetwork, @WinText2)
		
		EndDialog(hWin, IDCANCEL)
	End If
	
End Sub

Private Sub IDCANCEL_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	NetworkCleanUp()
	
	EndDialog(hWin, IDCANCEL)
	
End Sub

Private Sub IDOK_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	' Get Parameters from Dialog
	Scope
		' 1. %s Имя файла URL.
		' 2. %s Авторизация.
		' 3. %d Длина файла.
		' 4. %s Миме.
		' 5. %s Host.
		
		' [x] PUT {/filenape.png} HTTP/1.1
		' [x] Accept: */*
		' [ ] Authorization: Basic {Base64Auth}
		' [x] Connection: Close
		' [x] Content-Length: {123456}
		' [x] Content-Type: {application/binary}
		' [x] Host: {192.168.0.15}
		' [x] User-Agent: RestClient
			
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
			
			Dim sp As ServerWithPort = Any
			DivideServerPort( _
				@sp, _
				@bufServerPortName.szText(0), _
				ServerPortNameLength _
			)
			
			this->CRequest.ServerPort = sp.Port
			this->CRequest.ServerAddress = gethostbyname(sp.ServerName)
		End Scope
		
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
		
		Dim resSelect As Long = WSAAsyncSelect( _
			this->ClientSocket, _
			hWin, _
			WSA_NETEVENT, _
			FD_READ Or FD_WRITE Or FD_CLOSE Or FD_CONNECT _
		)
		If resSelect = SOCKET_ERROR Then
			Const Caption = __TEXT("WSAAsyncSelect")
			Dim dwError As Long = WSAGetLastError()
			closesocket(this->ClientSocket)
			CloseHandle(this->MapFileHandle)
			CloseHandle(this->FileHandle)
			this->ClientSocket = INVALID_SOCKET
			this->MapFileHandle = NULL
			this->FileHandle = INVALID_HANDLE_VALUE
			DisplayError(hWin, dwError, @Caption)
			Exit Sub
		End If
	End Scope
	
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
			If dwError <> WSAEWOULDBLOCK Then
				Const Caption = __TEXT("Connect to Server")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, dwError, @Caption)
				Exit Sub
			End If
		End If
	End Scope
	
	DisableDialogItem(hWin, IDOK)
	
End Sub

Private Sub Socket_OnWSANetEvent( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND, _
		ByVal nEvent As Long, _
		ByVal nError As Long _
	)
	
	Select Case nEvent
		
		Case FD_CONNECT
			If nError Then
				' CleanUp
				Const Caption = __TEXT("End of connect to Server")
				HttpRestFormCleanUp(this)
				DisplayError(hWin, nError, @Caption)
				
				EnableDialogItem(hWin, IDOK)
			End If
			
		Case FD_READ
			
		Case FD_WRITE
			' отправить данные
			' закрыть сокет
			
		Case FD_CLOSE
			
	End Select
	
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

Private Sub DialogMain_OnUnload( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	NetworkCleanUp()
	
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
					
			End Select
			
		Case WSA_NETEVENT
			Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))
			Dim nEvent As Long = WSAGETSELECTEVENT(lParam)
			Dim nError As Long = WSAGETSELECTERROR(lParam)
			Socket_OnWSANetEvent(pParam, hWin, nEvent, nError)
			
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
	
	Scope
		Dim hrVisualStyles As Integer = EnableVisualStyles()
		If FAILED(hrVisualStyles) Then
			Return 1
		End If
	End Scope
	
	Dim hWin As HWND = GetDesktopWindow()
	
	Dim param As HttpRestForm = Any
	With param
		.hInst = hInst
		.FileHandle = INVALID_HANDLE_VALUE
		.MapFileHandle = NULL
		.ClientSocket = INVALID_SOCKET
		.liFileSize.HighPart = 0
		.liFileSize.LowPart = 0
		.hHeap = GetProcessHeap()
	End With
	
	ZeroMemory(@param.CRequest, SizeOf(ClientRequest))
	
	Scope
		Dim DialogBoxParamResult As INT_PTR = DialogBoxParam( _
			hInst, _
			MAKEINTRESOURCE(IDD_DLG_TASKS), _
			hWin, _
			@InputDataDialogProc, _
			Cast(LPARAM, @param) _
		)
		
		If DialogBoxParamResult = -1 Then
			Const WinText2 = __TEXT("DialogBoxParam")
			Const WinCaption2 = __TEXT("Error")
			Dim dwError As DWORD = GetLastError()
			DisplayError(hWin, dwError, @WinText2)
			
			Return 1
		End If
		
	End Scope
	
	Return 0
	
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
