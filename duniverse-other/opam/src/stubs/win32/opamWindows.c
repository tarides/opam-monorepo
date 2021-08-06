/**************************************************************************/
/*                                                                        */
/*    Copyright 2015, 2016, 2017, 2018 MetaStack Solutions Ltd.           */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

#define CAML_NAME_SPACE
/* We need the UTF16 conversion functions */
#define CAML_INTERNALS
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/version.h>
#include <caml/osdeps.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

/* In a previous incarnation, dummy C stubs were generated for non-Windows
 * builds. Although this is no longer used, the C sources retain the ability to
 * be compiled this way. */
#ifdef _WIN32

#include <Windows.h>
#include <Shlobj.h>
#include <TlHelp32.h>

static struct custom_operations HandleOps =
{
  "org.ocaml.opam.Win32.Handle/1",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define HANDLE_val(v) (*((HANDLE*)Data_custom_val(v)))

typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);

static LPFN_ISWOW64PROCESS IsWoW64Process = NULL;

static inline BOOL has_IsWoW64Process(void)
{
  return (IsWoW64Process
          || (IsWoW64Process =
               (LPFN_ISWOW64PROCESS)GetProcAddress(GetModuleHandle("kernel32"),
                                                   "IsWow64Process")));
}

/*
 * Taken from otherlibs/win32unix/winwait.c (sadly declared static)
 * Altered only for CAML_NAME_SPACE
 */
static value alloc_process_status(HANDLE pid, int status)
{
  value res, st;

  st = caml_alloc(1, 0);
  Field(st, 0) = Val_int(status);
  Begin_root (st);
    res = caml_alloc_small(2, 0);
    Field(res, 0) = Val_long((intnat) pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}

/* Order must match OpamStubsTypes.registry_root */
static HKEY roots[] =
  {HKEY_CLASSES_ROOT,
   HKEY_CURRENT_CONFIG,
   HKEY_CURRENT_USER,
   HKEY_LOCAL_MACHINE,
   HKEY_USERS};

/*
 * OPAMW_process_putenv is implemented using Process Injection.
 * Idea inspired by Bill Stewart's editvar
 *   (see http://www.westmesatech.com/editv.html)
 * Full technical details at http://www.codeproject.com/Articles/4610/Three-Ways-to-Inject-Your-Code-into-Another-Proces#section_3
 */

static char* getProcessInfo(HANDLE hProcessSnapshot,
                            DWORD processId,
                            PROCESSENTRY32 *entry)
{
  entry->dwSize = sizeof(PROCESSENTRY32);

  if (hProcessSnapshot == INVALID_HANDLE_VALUE)
    return "getProcessInfo: could not create snapshot";

  /*
   * Locate our process
   */
  if (!Process32First(hProcessSnapshot, entry))
  {
    CloseHandle(hProcessSnapshot);
    return "getProcessInfo: could not walk process tree";
  }
  else
  {
    while (entry->th32ProcessID != processId)
    {
      if (!Process32Next(hProcessSnapshot, entry))
      {
        CloseHandle(hProcessSnapshot);
        return "getProcessInfo: could not find process!";
      }
    }
  }

  return NULL;
}

char* InjectSetEnvironmentVariable(DWORD pid, const char* key, const char* val);

#define OPAMreturn CAMLreturn

#else

#define OPAMreturn(v) CAMLreturn(Val_unit)

#endif

/* Actual primitives from here */
CAMLprim value OPAMW_GetCurrentProcessID(value unit)
{
  CAMLparam1(unit);

  OPAMreturn(caml_copy_int32(GetCurrentProcessId()));
}

CAMLprim value OPAMW_GetStdHandle(value nStdHandle)
{
  CAMLparam1(nStdHandle);
#ifdef _WIN32
  CAMLlocal1(result);

  HANDLE hResult;

  if ((hResult = GetStdHandle(-Int_val(nStdHandle) - 10)) == NULL)
    caml_raise_not_found();

  result = caml_alloc_custom(&HandleOps, sizeof(HANDLE), 0, 1);
  HANDLE_val(result) = hResult;
#endif

  OPAMreturn(result);
}

CAMLprim value OPAMW_GetConsoleScreenBufferInfo(value hConsoleOutput)
{
  CAMLparam1(hConsoleOutput);
#ifdef _WIN32
  CAMLlocal2(result, coord);

  CONSOLE_SCREEN_BUFFER_INFO buffer;

  if (!GetConsoleScreenBufferInfo(HANDLE_val(hConsoleOutput), &buffer))
    caml_raise_not_found();

  result = caml_alloc(5, 0);
  coord = caml_alloc(2, 0);
  Store_field(coord, 0, Val_int(buffer.dwSize.X));
  Store_field(coord, 1, Val_int(buffer.dwSize.Y));
  Store_field(result, 0, coord);
  coord = caml_alloc(2, 0);
  Store_field(coord, 0, Val_int(buffer.dwCursorPosition.X));
  Store_field(coord, 1, Val_int(buffer.dwCursorPosition.Y));
  Store_field(result, 1, coord);
  Store_field(result, 2, Val_int(buffer.wAttributes));
  coord = caml_alloc(4, 0);
  Store_field(coord, 0, Val_int(buffer.srWindow.Left));
  Store_field(coord, 1, Val_int(buffer.srWindow.Top));
  Store_field(coord, 2, Val_int(buffer.srWindow.Right));
  Store_field(coord, 3, Val_int(buffer.srWindow.Bottom));
  Store_field(result, 3, coord);
  coord = caml_alloc(2, 0);
  Store_field(coord, 0, Val_int(buffer.dwMaximumWindowSize.X));
  Store_field(coord, 1, Val_int(buffer.dwMaximumWindowSize.Y));
  Store_field(result, 4, coord);
#endif

  OPAMreturn(result);
}

CAMLprim value OPAMW_SetConsoleTextAttribute(value hConsoleOutput,
                                             value wAttributes)
{
  CAMLparam2(hConsoleOutput, wAttributes);

#ifdef _WIN32
  if (!SetConsoleTextAttribute(HANDLE_val(hConsoleOutput),
                               Int_val(wAttributes)))
    caml_failwith("setConsoleTextAttribute");
#endif

  OPAMreturn(Val_unit);
}

CAMLprim value OPAMW_FillConsoleOutputCharacter(value vhConsoleOutput,
                                                value character,
                                                value vnLength,
                                                value vdwWriteCoord)
{
  CAMLparam4(vhConsoleOutput, character, vnLength, vdwWriteCoord);

#ifdef _WIN32
  HANDLE hConsoleOutput = HANDLE_val(vhConsoleOutput);
  CONSOLE_SCREEN_BUFFER_INFO ConsoleScreenBufferInfo;
  WCHAR cCharacter = Int_val(character) & 0xFF;
  DWORD nLength = Int_val(vnLength);
  COORD dwWriteCoord =
    {Int_val(Field(vdwWriteCoord, 0)), Int_val(Field(vdwWriteCoord, 1))};
  DWORD dwNumberOfCharsWritten;
  BOOL result = FALSE;

  if (GetConsoleScreenBufferInfo(hConsoleOutput, &ConsoleScreenBufferInfo))
  {
    while ((result = FillConsoleOutputCharacter(hConsoleOutput,
                                                cCharacter,
                                                nLength,
                                                dwWriteCoord,
                                                &dwNumberOfCharsWritten))
           && dwNumberOfCharsWritten != nLength)
    {
      nLength -= dwNumberOfCharsWritten;
      dwWriteCoord.X += dwNumberOfCharsWritten;
      dwWriteCoord.Y += dwWriteCoord.X / ConsoleScreenBufferInfo.dwSize.X;
      dwWriteCoord.X %= ConsoleScreenBufferInfo.dwSize.X;
    }
  }
#endif

  OPAMreturn(Val_bool(result));
}

CAMLprim value OPAMW_GetConsoleMode(value hConsoleHandle)
{
  CAMLparam1(hConsoleHandle);

#ifdef _WIN32
  DWORD dwMode;
  if (!GetConsoleMode(HANDLE_val(hConsoleHandle), &dwMode))
#endif
    caml_raise_not_found();

  OPAMreturn(Val_int(dwMode));
}

CAMLprim value OPAMW_SetConsoleMode(value hConsoleMode, value dwMode)
{
  CAMLparam2(hConsoleMode, dwMode);

#ifdef _WIN32
  BOOL result = SetConsoleMode(HANDLE_val(hConsoleMode), Int_val(dwMode));
#endif

  OPAMreturn(Val_bool(result));
}

CAMLprim value OPAMW_GetWindowsVersion(value unit)
{
  CAMLparam1(unit);

#ifdef _WIN32
  CAMLlocal1(result);
  result = caml_alloc_tuple(4);
#if OCAML_VERSION >= 40600
  Store_field(result, 0, Val_int(caml_win32_major));
  Store_field(result, 1, Val_int(caml_win32_minor));
  Store_field(result, 2, Val_int(caml_win32_build));
  Store_field(result, 3, Val_int(caml_win32_revision));
#else
  Store_field(result, 0, Val_int(0));
  Store_field(result, 1, Val_int(0));
  Store_field(result, 2, Val_int(0));
  Store_field(result, 3, Val_int(0));
#endif
#endif

  OPAMreturn(result);
}

CAMLprim value OPAMW_IsWoW64(value unit)
{
  CAMLparam1(unit);

#ifdef _WIN32
  BOOL result = FALSE;
  /*
   * 32-bit versions may or may not have IsWow64Process (depends on age).
   * Recommended way is to use GetProcAddress to obtain IsWow64Process, rather
   * than relying on Windows.h.
   * See http://msdn.microsoft.com/en-gb/library/windows/desktop/ms684139.aspx
   */
  if (has_IsWoW64Process() && !IsWoW64Process(GetCurrentProcess(), &result))
    result = FALSE;
#endif

  OPAMreturn(Val_bool(result));
}

/*
 * Adapted from otherlibs/win32unix/winwait.c win_waitpid
 */
CAMLprim value OPAMW_waitpids(value vpid_reqs, value vpid_len)
{
#ifdef _WIN32
  int i;
  DWORD status, retcode;
  HANDLE pid_req;
  DWORD err = 0;
  int len = Int_val(vpid_len);
  HANDLE *lpHandles = (HANDLE*)malloc(sizeof(HANDLE) * len);
  value ptr = vpid_reqs;

  if (lpHandles == NULL)
    caml_raise_out_of_memory();

  for (i = 0; i < len; i++) {
    lpHandles[i] = (HANDLE)Long_val(Field(ptr, 0));
    ptr = Field(ptr, 1);
  }

  caml_enter_blocking_section();
  retcode = WaitForMultipleObjects(len, lpHandles, FALSE, INFINITE);
  if (retcode == WAIT_FAILED) err = GetLastError();
  caml_leave_blocking_section();
  if (err) {
    win32_maperr(err);
    uerror("waitpids", Nothing);
  }
  pid_req = lpHandles[retcode - WAIT_OBJECT_0];
  free(lpHandles);
  if (! GetExitCodeProcess(pid_req, &status)) {
    win32_maperr(GetLastError());
    uerror("waitpids", Nothing);
  }

  /*
   * NB Unlike in win_waitpid, it's not possible to have status == STILL_ACTIVE
   */
  CloseHandle(pid_req);
  return alloc_process_status(pid_req, status);
#else
  return Val_unit;
#endif
}

CAMLprim value OPAMW_WriteRegistry(value hKey,
                                   value lpSubKey,
                                   value lpValueName,
                                   value dwType,
                                   value lpData)
{
  CAMLparam5(hKey, lpSubKey, lpValueName, dwType, lpData);

#ifdef _WIN32
  HKEY key;
  const void* buf = NULL;
  DWORD cbData = 0;
  DWORD type = 0;

  switch (RegOpenKeyEx(roots[Int_val(hKey)],
                       String_val(lpSubKey),
                       0,
                       KEY_WRITE,
                       &key))
  {
    case ERROR_SUCCESS:
      {
        /* Cases match OpamStubsTypes.registry_value */
        switch (Int_val(dwType))
        {
          case 0:
            {
              buf = String_val(lpData);
              cbData = strlen(buf) + 1;
              type = REG_SZ;
              break;
            }
          default:
            {
              caml_failwith("OPAMW_WriteRegistry: value not implemented");
              break;
            }
        }
        if (RegSetValueEx(key,
                          String_val(lpValueName),
                          0,
                          type,
                          (LPBYTE)buf,
                          cbData) != ERROR_SUCCESS)
        {
          RegCloseKey(key);
          caml_failwith("RegSetValueEx");
        }
        RegCloseKey(key);
        break;
      }
    case ERROR_FILE_NOT_FOUND:
      {
        caml_raise_not_found();
        break;
      }
    default:
      {
        caml_failwith("RegOpenKeyEx");
        break;
      }
  }
#endif

  OPAMreturn(Val_unit);
}

CAMLprim value OPAMW_GetConsoleOutputCP(value unit)
{
  CAMLparam1(unit);

  OPAMreturn(Val_int(GetConsoleOutputCP()));
}

CAMLprim value OPAMW_GetCurrentConsoleFontEx(value hConsoleOutput,
                                             value bMaximumWindow)
{
  CAMLparam2(hConsoleOutput, bMaximumWindow);
#ifdef _WIN32
  CAMLlocal3(result, coord, name);

  int len;
  CONSOLE_FONT_INFOEX fontInfo;
  fontInfo.cbSize = sizeof(fontInfo);

  if (GetCurrentConsoleFontEx(HANDLE_val(hConsoleOutput),
                              Bool_val(bMaximumWindow),
                              &fontInfo))
  {
    result = caml_alloc(5, 0);
    Store_field(result, 0, Val_int(fontInfo.nFont));
    coord = caml_alloc(2, 0);
    Store_field(coord, 0, Val_int(fontInfo.dwFontSize.X));
    Store_field(coord, 0, Val_int(fontInfo.dwFontSize.Y));
    Store_field(result, 1, coord);
    Store_field(result, 2, Val_int(fontInfo.FontFamily));
    Store_field(result, 3, Val_int(fontInfo.FontWeight));
    Store_field(result, 4, caml_copy_string_of_utf16(fontInfo.FaceName));
  }
  else
  {
    caml_raise_not_found();
  }
#endif

  OPAMreturn(result);
}

CAMLprim value OPAMW_CreateGlyphChecker(value fontName)
{
  CAMLparam1(fontName);
#ifdef _WIN32
  CAMLlocal2(result, handle);

  /*
   * Any device context will do to load the font, so use the Screen DC.
   */
  HDC hDC = GetDC(NULL);

  if (hDC)
  {
    wchar_t* lpszFace = caml_stat_strdup_to_utf16(String_val(fontName));
    HFONT hFont =
      CreateFontW(0, 0, 0, 0, FW_DONTCARE, FALSE, FALSE, FALSE, DEFAULT_CHARSET,
                  OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                  DEFAULT_PITCH, lpszFace);
    caml_stat_free(lpszFace);

    if (hFont)
    {
      if (SelectObject(hDC, hFont))
      {
        result = caml_alloc_tuple(2);
        handle = caml_alloc_custom(&HandleOps, sizeof(HANDLE), 0, 1);
        HANDLE_val(handle) = hDC;
        Store_field(result, 0, handle);
        handle = caml_alloc_custom(&HandleOps, sizeof(HANDLE), 0, 1);
        HANDLE_val(handle) = hFont;
        Store_field(result, 1, handle);
      }
      else
      {
        caml_failwith("OPAMW_CheckGlyphs: SelectObject");
      }
    }
    else
    {
      caml_failwith("OPAMW_CheckGlyphs: CreateFontW");
    }
  }
  else
  {
    caml_failwith("OPAMW_CheckGlyphs: GetDC");
  }
#endif

  OPAMreturn(result);
}

CAMLprim value OPAMW_DeleteGlyphChecker(value checker)
{
  CAMLparam1(checker);

#ifdef _WIN32
  DeleteObject(HANDLE_val(Field(checker, 1)));
  ReleaseDC(NULL, HANDLE_val(Field(checker, 0)));
#endif

  CAMLreturn(Val_unit);
}

CAMLprim value OPAMW_HasGlyph(value checker, value scalar)
{
  CAMLparam2(checker, scalar);
#ifdef _WIN32
  BOOL result = FALSE;
  HDC hDC = HANDLE_val(Field(checker, 0));

  WCHAR test = (WCHAR)Int_val(scalar);
  WORD index = 0;

  switch (GetGlyphIndicesW(hDC, &test, 1, &index, GGI_MARK_NONEXISTING_GLYPHS))
  {
    case 1:
      break;
    case GDI_ERROR:
      caml_failwith("OPAMW_CheckGlyphs: GetGlyphIndicesW");
    default:
      caml_failwith("OPAMW_CheckGlyphs: GetGlyphIndicesW (unexpected return)");
  }
#endif

  OPAMreturn(Val_bool(index != 0xffff));
}

CAMLprim value OPAMW_process_putenv(value pid, value key, value val)
{
  CAMLparam3(pid, key, val);
#ifdef _WIN32
  CAMLlocal1(res);

  char* result;

  /*
   * MSDN is all over the place as to what the technical limits are for
   * environment variables (looks like 32KiB for both both name and value)
   * however there's no need to inject 64KiB data each time - hence 4KiB limit.
   */
  if (caml_string_length(key) > 4095 || caml_string_length(val) > 4095)
    caml_invalid_argument("Strings too long");

  result =
    InjectSetEnvironmentVariable(Int32_val(pid),
                                 String_val(key),
                                 String_val(val));

  if (result == NULL)
  {
    res = Val_true;
  }
  else if (strlen(result) == 0)
  {
    res = Val_false;
  }
  else
  {
    caml_failwith(result);
  }
#endif

  OPAMreturn(res);
}

CAMLprim value OPAMW_IsWoW64Process(value pid)
{
  CAMLparam1(pid);

#ifdef _WIN32
  BOOL result = FALSE;

  if (has_IsWoW64Process())
  {
    HANDLE hProcess =
      OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, Int32_val(pid));

    if (hProcess)
    {
      if (!IsWoW64Process(hProcess, &result))
        result = FALSE;
      CloseHandle(hProcess);
    }
  }
#endif

  OPAMreturn(Val_bool(result));
}

/*
 * Somewhat against my better judgement, wrap SHGetFolderPath rather than
 * SHGetKnownFolderPath to maintain XP compatibility. OPAM already requires
 * Windows Vista+ because of GetCurrentConsoleFontEx, but there may be a
 * workaround for that for XP lusers.
 */
CAMLprim value OPAMW_SHGetFolderPath(value nFolder, value dwFlags)
{
  CAMLparam2(nFolder, dwFlags);
#ifdef _WIN32
  CAMLlocal1(result);
  TCHAR szPath[MAX_PATH];

  if (SUCCEEDED(SHGetFolderPath(NULL,
                                Int_val(nFolder),
                                NULL,
                                Int_val(dwFlags),
                                szPath)))
    result = caml_copy_string(szPath);
  else
    caml_failwith("OPAMW_SHGetFolderPath");
#endif

  OPAMreturn(result);
}

CAMLprim value OPAMW_SendMessageTimeout(value hWnd,
                                        value uTimeout,
                                        value fuFlags,
                                        value vmsg,
                                        value vwParam,
                                        value vlParam)
{
  CAMLparam5(hWnd, vmsg, vwParam, vlParam, fuFlags);
  CAMLxparam1(uTimeout);
#ifdef _WIN32
  CAMLlocal1(result);

  DWORD_PTR dwReturnValue;
  HRESULT lResult;
  WPARAM wParam;
  LPARAM lParam;
  UINT msg;

  switch (Int_val(vmsg))
  {
    case 0:
      {
        msg = WM_SETTINGCHANGE;
        wParam = Int_val(vwParam);
        lParam = (LPARAM)String_val(vlParam);
        break;
      }
    default:
      {
        caml_failwith("OPAMW_SendMessageTimeout: message not implemented");
        break;
      }
  }

  lResult =
    SendMessageTimeout((HWND)Nativeint_val(hWnd),
                        msg,
                        wParam,
                        lParam,
                        Int_val(fuFlags),
                        Int_val(uTimeout),
                        &dwReturnValue);

  switch (Int_val(vmsg))
  {
    case 0:
      {
        result = caml_alloc(2, 0);
        Store_field(result, 0, Val_int(lResult));
        Store_field(result, 1, Val_int(dwReturnValue));
        break;
      }
  }
#endif

  OPAMreturn(result);
}

CAMLprim value OPAMW_SendMessageTimeout_byte(value * v, int n)
{
  return OPAMW_SendMessageTimeout(v[0], v[1], v[2], v[3], v[4], v[5]);
}

CAMLprim value OPAMW_GetParentProcessID(value processId)
{
  CAMLparam1(processId);

#ifdef _WIN32
  PROCESSENTRY32 entry;
  char* msg;
  /*
   * Create a Toolhelp Snapshot of running processes
   */
  HANDLE hProcessSnapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  if ((msg = getProcessInfo(hProcessSnapshot, Int32_val(processId), &entry)))
    caml_failwith(msg);

  /*
   * Finished with the snapshot
   */
  CloseHandle(hProcessSnapshot);
#endif

  OPAMreturn(caml_copy_int32(entry.th32ParentProcessID));
}

CAMLprim value OPAMW_GetConsoleAlias(value alias, value exeName)
{
  CAMLparam2(alias, exeName);
#ifdef _WIN32
  CAMLlocal1(result);

  DWORD nLength = 8192;
  LPTSTR buffer = (LPTSTR)malloc(nLength);

  if (!buffer)
    caml_raise_out_of_memory();

  if (GetConsoleAlias((LPTSTR)String_val(alias), buffer, nLength,
                      (LPTSTR)String_val(exeName)))
  {
    result = caml_copy_string(buffer);
  }
  else
  {
    result = caml_copy_string("");
  }

  free(buffer);
#endif

  OPAMreturn(result);
}
