#include once "WinMain.bi"
#include once "win\commctrl.bi"
#include once "win\shlobj.bi"
#include once "win\windowsx.bi"
#include once "Resources.RH"

Const C_COLUMNS As UINT = 2

Type HttpRestForm
	hInst As HINSTANCE
End Type

Type ResStringBuffer
	szText(255) As TCHAR
End Type

Type MainFormParam
	hWin As HWND
	cFileName(MAX_PATH) As TCHAR
End Type

Private Sub DialogMain_OnLoad( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	
End Sub

Private Sub ButtonAdd_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	
End Sub

Private Sub ButtonStart_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	
End Sub

Private Sub ButtonStop_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	
End Sub

Private Sub ButtonRemove_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	
End Sub

Private Sub IDCANCEL_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	PostQuitMessage(0)
	
End Sub

Private Sub ButtonClear_OnClick( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	
End Sub

Private Sub DialogMain_OnUnload( _
		ByVal this As HttpRestForm Ptr, _
		ByVal hWin As HWND _
	)
	
	
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
				
				Case IDC_BTN_ADD
					ButtonAdd_OnClick(pParam, hWin)
					
				Case IDC_BTN_START
					ButtonStart_OnClick(pParam, hWin)
					
				Case IDC_BTN_STOP
					ButtonStop_OnClick(pParam, hWin)
					
				Case IDC_BTN_REMOVE
					ButtonRemove_OnClick(pParam, hWin)
					
				Case IDC_BTN_CLEAR
					ButtonClear_OnClick(pParam, hWin)
					
				Case IDCANCEL
					IDCANCEL_OnClick(pParam, hWin)
					
			End Select
			
		Case WM_NOTIFY
			' Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))
			' Dim pHdr As NMHDR Ptr = Cast(NMHDR Ptr, lParam)
			
			' Select Case pHdr->code
				
			' 	Case NM_CLICK
			' 		Dim lpnmitem As NMITEMACTIVATE Ptr = Cast(NMITEMACTIVATE Ptr, lParam)
			' 		ListView_OnClick(pParam, hWin, pHdr->hwndFrom, lpnmitem)
					
			' End Select
			
		' Case WM_CLOSE
		' 	Dim pParam As HttpRestForm Ptr = Cast(HttpRestForm Ptr, GetWindowLongPtr(hWin, GWLP_USERDATA))
		' 	DialogMain_OnUnload(pParam, hWin)
		' 	PostQuitMessage(0)
			
		Case Else
			Return FALSE
			
	End Select
	
	Return TRUE
	
End Function

Private Function EnableVisualStyles()As HRESULT
	
	Dim icc As INITCOMMONCONTROLSEX = Any
	icc.dwSize = SizeOf(INITCOMMONCONTROLSEX)
	icc.dwICC = ICC_ANIMATE_CLASS Or _
		ICC_BAR_CLASSES Or _
		ICC_COOL_CLASSES Or _
		ICC_DATE_CLASSES Or _
		ICC_HOTKEY_CLASS Or _
		ICC_INTERNET_CLASSES Or _
		ICC_LINK_CLASS Or _
		ICC_LISTVIEW_CLASSES Or _
		ICC_NATIVEFNTCTL_CLASS Or _
		ICC_PAGESCROLLER_CLASS Or _
		ICC_PROGRESS_CLASS Or _
		ICC_STANDARD_CLASSES Or _
		ICC_TAB_CLASSES Or _
		ICC_TREEVIEW_CLASSES Or _
		ICC_UPDOWN_CLASS Or _
		ICC_USEREX_CLASSES Or _
	ICC_WIN95_CLASSES
	
	Dim res As BOOL = InitCommonControlsEx(@icc)
	If res = 0 Then
		Dim dwError As DWORD = GetLastError()
		Return HRESULT_FROM_WIN32(dwError)
	End If
	
	Return S_OK
	
End Function

Private Function tWinMain( _
		Byval hInst As HINSTANCE, _
		ByVal hPrevInstance As HINSTANCE, _
		ByVal lpCmdLine As LPTSTR, _
		ByVal iCmdShow As Long _
	)As Integer
	
	
	Const WinText = __TEXT("Hello from FreeBASIC")
	Const WinCaption = __TEXT("Test GUI")
	
	Dim hWin As HWND = GetDesktopWindow()
	MessageBox(hWin, @WinText, @WinCaption, MB_OK Or MB_ICONINFORMATION)
	
	Scope
		Dim hrVisualStyles As Integer = EnableVisualStyles()
		If FAILED(hrVisualStyles) Then
			Return 1
		End If
	End Scope
	
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
			
			MessageBox(hWin, @WinText2, @WinCaption2, MB_OK Or MB_ICONINFORMATION)
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
		' ExitProcess(RetCode)
	#endif
	
	Return RetCode
	
End Function

#ifndef WITHOUT_RUNTIME
Dim RetCode As Long = CLng(EntryPoint())
End(RetCode)
#endif
