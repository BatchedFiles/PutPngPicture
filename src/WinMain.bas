#include once "WinMain.bi"
#include once "win\commctrl.bi"
#include once "win\commdlg.bi"
#include once "win\shlobj.bi"
#include once "win\windowsx.bi"
#include once "win\mswsock.bi"
#include once "Resources.RH"

Const C_COLUMNS As UINT = 2

Type HttpRestForm
	hInst As HINSTANCE
End Type

Type ResStringBuffer
	szText(255) As TCHAR
End Type

Type FileNameBuffer
	szText(MAX_PATH) As TCHAR
End Type

Type MainFormParam
	hWin As HWND
	cFileName(MAX_PATH) As TCHAR
End Type

Type ErrorBuffer
	szText(255) As TCHAR
End Type

Private Sub DisplayError( _
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
	
	MessageBox(0, @buf.szText(0), Caption, MB_OK Or MB_ICONERROR)
	
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
		
		DisplayError(hrInitNetwork, @WinText2)
		
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
			DisplayError(dwError, @WinText2)
			
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
	param.hInst = hInst
	
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
			DisplayError(dwError, @WinText2)
			
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
