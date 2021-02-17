// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFLazarusBrowserWindow;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uCEFApplication, uCEFChromiumWindow, uCEFTypes, uCEFInterfaces, Forms,
  ExtCtrls, Classes, sysutils;

type

  { TLazarusBrowserWindow }

  TLazarusBrowserWindow = class(TChromiumWindow)
  private
    FCanClose  : boolean;  // Set to True in TChromium.OnBeforeClose
    FLoadUrl: ustring;
    FOnBrowserClosed: TNotifyEvent;
    FOnBrowserCreated: TNotifyEvent;
    FTimer: TTimer;

    procedure DoCreateBrowser(Sender: TObject);
  protected
    procedure DestroyHandle; override;
    procedure RealizeBounds; override;

    procedure WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction); reintroduce;
    procedure WebBrowser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser); reintroduce;

    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    procedure CreateHandle; override;

    procedure CloseBrowser(aForceClose: boolean); reintroduce;
    procedure LoadURL(aURL: ustring); reintroduce;

  published
    property OnBrowserCreated: TNotifyEvent read FOnBrowserCreated write FOnBrowserCreated;
    property OnBrowserClosed: TNotifyEvent read FOnBrowserClosed write FOnBrowserClosed;
  end;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

implementation

{ TLazarusBrowserWindow }

procedure TLazarusBrowserWindow.DoCreateBrowser(Sender: TObject);
begin
  FCanClose := False;
  FTimer.Enabled := False;

  if (CreateBrowser and Initialized) then begin
    FreeAndNil(FTimer);
    {$IFDEF FPC}{$IFDEF LINUX}
    ChromiumBrowser.UpdateXWindowVisibility(Visible);
    ChromiumBrowser.UpdateBrowserSize(Left, Top, Width, Height);
    {$ENDIF}{$ENDIF}

    if FOnBrowserCreated <> nil then
      FOnBrowserCreated(Self);
    if FLoadUrl <> '' then
      LoadURL(FLoadUrl);
  end
  else begin
    if GlobalCEFApp.ExternalMessagePump then
      GlobalCEFApp.DoMessageLoopWork;

    FTimer.Enabled := True;
  end;
end;

procedure TLazarusBrowserWindow.CreateHandle;
begin
  inherited CreateHandle;
  if not (csDesigning in ComponentState) then begin
    FTimer := TTimer.Create(Self);
    FTimer.Interval := 20;
    FTimer.OnTimer := @DoCreateBrowser;
    FTimer.Enabled := True;
  end;
end;

procedure TLazarusBrowserWindow.DestroyHandle;
begin
  if FTimer <> nil then
    FreeAndNil(FTimer);

  if (GlobalCEFApp = nil) or FCanClose or
     (csDesigning in ComponentState)
  then begin
    inherited DestroyHandle;
    exit;
  end;

  CloseBrowser(True);
  while not FCanClose do begin
    Application.ProcessMessages;
    if GlobalCEFApp.ExternalMessagePump then
      GlobalCEFApp.DoMessageLoopWork;
    sleep(5);
  end;

  inherited DestroyHandle;

  Application.ProcessMessages;
  if GlobalCEFApp.ExternalMessagePump then
    GlobalCEFApp.DoMessageLoopWork;
end;

procedure TLazarusBrowserWindow.RealizeBounds;
begin
  inherited RealizeBounds;

  if not (csDesigning in ComponentState) and HandleAllocated then
    NotifyMoveOrResizeStarted;
end;

procedure TLazarusBrowserWindow.WebBrowser_OnClose(Sender: TObject;
  const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  if DestroyChildWindow then
    aAction := cbaDelay
  else
    aAction := cbaClose;
end;

procedure TLazarusBrowserWindow.WebBrowser_OnBeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  FCanClose := True;
  if FOnBrowserClosed <> nil then
    FOnBrowserClosed(Self);
end;

procedure TLazarusBrowserWindow.DoEnter;
begin
  inherited DoEnter;
  If not(csDesigning in ComponentState) then ChromiumBrowser.SetFocus(True);
end;

procedure TLazarusBrowserWindow.DoExit;
begin
  inherited DoExit;
  if not(csDesigning in ComponentState) then
    ChromiumBrowser.SendCaptureLostEvent;
end;

constructor TLazarusBrowserWindow.Create(AOwner: TComponent);
begin
  FCanClose := True;
  inherited Create(AOwner);
end;

procedure TLazarusBrowserWindow.AfterConstruction;
begin
  inherited AfterConstruction;
  if not(csDesigning in ComponentState) then begin
    FChromium.OnClose        := @WebBrowser_OnClose;
    FChromium.OnBeforeClose  := @WebBrowser_OnBeforeClose;
  end;
end;

procedure TLazarusBrowserWindow.CloseBrowser(aForceClose: boolean);
begin
  if (GlobalCEFApp = nil) or FCanClose or (not HandleAllocated) then begin
    if FOnBrowserClosed <> nil then
      FOnBrowserClosed(Self);
    exit;
  end;
  inherited CloseBrowser(aForceClose);
end;

procedure TLazarusBrowserWindow.LoadURL(aURL: ustring);
begin
  FLoadUrl := '';
  if not Initialized then
    FLoadUrl := aURL
  else
    inherited LoadURL(aURL);
end;

{$IFDEF FPC}
procedure Register;
begin
  {$I res/tlazarusbrowserwindow.lrs}
  RegisterComponents('Chromium', [TLazarusBrowserWindow]);
end;
{$ENDIF}

end.

