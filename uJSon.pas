unit uJSon;

interface
uses
    System.Json, REST.Types, System.SysUtils, System.StrUtils, System.Generics.Collections,
    FMX.Graphics,
        System.Generics.Defaults, System.Classes, System.DateUtils,
        REST.Client,
        uClasses, uUtils, uGlobal, uConst,
        FMX.Features.BitmapHelper;
    /// <summary>
    /// Создает запрос для авторизации пользователя
    /// </summary>
    function CreateAutchPacket(grant_type, username, password : string) : string;
    function ParseAutchPacket(AJSONValue: TJSONValue) : boolean;

    function CreateExtraAutchPacket(socialnetwork, token : string) : string;
    //function ParseExtraAutchPacket(AJSONValue: TJSONValue) : boolean;

    function CreateRegisterPacket(login, password : string; gender, socialstatus : integer) : string;
    //function ParseRegisterPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг информации пользователя
    /// </summary>
    function ParseUserItem(AJSONValue: TJSONValue; const pUser : Pointer) : boolean;
    /// <summary>
    /// Парсинг справочников (города, гендер и пр)
    /// </summary>
    function ParseDictonaryItem(var dictonaryItem : TDictonaryItem; AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг Списка баров
    /// </summary>
    function ParseBars(var bars : TList<TBar>; AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг информации по бару
    /// </summary>
    function ParseBarItem(AJSONValue: TJSONValue) : TBar;
    /// <summary>
    /// Парсинг описания бара
    /// </summary>
    function ParseBarDescription(var barDescription : TBarDescription; AJSONObject: TJSONPair) : boolean;
    /// <summary>
    /// Парсинг Списка квестов
    /// </summary>
    function ParseQuest(var quests : TList<TQuest>; AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг квеста
    /// </summary>
    function ParseQuestItem(AJSONValue: TJSONValue) : TQuest;
    /// <summary>
    /// Парсинг информации по квесту
    /// </summary>
    function ParseQuestInfo(var questInfo : TQuestInfo; AJSONObject: TJSONPair) : boolean;
    /// <summary>
    /// Парсинг описания квеста
    /// </summary>
    function ParseQuestDescription(var questDescription : TQuestDescription; AJSONObject: TJSONPair) : boolean;

    /// <summary>
    /// Создание запроса на редактирование профиля
    /// </summary>              TProfileEdit
    function CreateProfileEditPacket(profileEdit : TProfileEdit) : string;
    /// <summary>
    /// Создание запроса на восстановления пароля
    /// </summary>
    function CreateResendPassword(email, guid, text : string) : string;
    /// <summary>
    /// Создание запроса на загрузку аватара пользователя
    /// </summary>
    function CreateUploadAvatarPacket(image, filename : string) : string;
    /// <summary>
    /// Парсинг справочника уровней
    /// </summary>
    function ParseGetAllLevelsPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг справочника городов
    /// </summary>
    function ParseGetAllCitiesPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг справочника городов
    /// </summary>
    function ParseGetAllSocialStatusesPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг справочника городов
    /// </summary>
    function ParseGetAllRelationshipsPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг справочника тип акции
    /// </summary>
    function ParseGetAllRaritiesPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг справочника статуса квеста
    /// </summary>
    function ParseGetAllQuestStatusesPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг справочника
    /// </summary>
    function ParseGetDictionaryPacket(AJSONValue: TJSONValue; var aList : TDictionaryList;//TList<TDictonaryItem>;
        isSort : boolean = true) : boolean;
    /// <summary>
    /// Создание запроса на отмену активной акции пользователя
    /// </summary>
    function CreateCancelActiveQuestPacket(UserActiveQuestId : integer) : string;
    /// <summary>
    /// Парсинг Активных квестов
    /// </summary>
    function ParseActiveQuestPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг выполненных квестов
    /// </summary>
    function ParseCompletedQuestsPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг выполненных квестов
    /// </summary>
    function ParseCompletedQuests(var completedQuests : TList<TQuest>; AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг выполненного квеста
    /// </summary>
    function ParseCompletedQuestItem(AJSONValue: TJSONValue) : TQuest;
    /// <summary>
    /// Парсинг логотипа/картинки
    /// </summary>
    function ParseLogo(plogo :Pointer; AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Создание запроса на оставление отзыва бара
    /// </summary>
    function CreateBarSendCommentPacket(comment : TComment) : string;
    /// <summary>
    /// Парсинг отзывов по бару
    /// </summary>
    function ParseBarCommentsPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг отзыва по бару
    /// </summary>
    function ParseCommentItem(AJSONValue: TJSONValue) : TComment;
    /// <summary>
    /// Парсинг друзей
    /// </summary>
    function ParseFriendsPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг друзей
    /// </summary>
    function ParseUsers(AJSONValue: TJSONValue; var list : TList<TUserInfo>) : boolean;
    /// <summary>
    /// Парсинг пользователей при поиске по логину
    /// </summary>
    function ParseUserListPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг информации текущего профиля
    /// </summary>
    function ParseProfilePacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг подписчиков
    /// </summary>
    function ParseFollowersPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг приглашенных друзей (не подтвердили)
    /// </summary>
    function ParseFriendsInvitedPacket(AJSONValue: TJSONValue) : boolean;
    /// <summary>
    /// Парсинг капчи
    /// </summary>
    function ParseCaptcha(AJSONValue: TJSONValue) : boolean;
    function ParseCaptchaName(strings : TStrings) : Boolean;
    /// <summary>
    /// Создание запроса на получения qr-coda
    /// </summary>
    function CreateQrCode(userId, questId : string; deviceType : string = '';
        token : string = ''; androidDeviceid : string = '';
        iosSertPassw : string = '') : string;
    /// <summary>
    /// Парсинг изменений пользователя
    /// </summary>
    function ParseChangeActions(AJSONValue: TJSONValue) : boolean;
    function ParseChangeTime(strings : TStrings) : Boolean;
    /// <summary>
    /// Парсинг врмени сервера
    /// </summary>
    function ParseServerTime(strings : TStrings) : Boolean;
    /// <summary>
    /// Парсинг массива и запись в лист
    /// </summary>
    function ParseListItem(AJSONValue: TJSONValue; parse : TProc<TJSONValue>) : boolean;
    /// <summary>
    /// Создание запроса на CheckIn
    /// </summary>
    function CreateCheckIn(barId : string) : string;

implementation
{------------------------------------------------------------------------------}
{$REGION 'Создание и парсинг запроса авторизации'}
function CreateAutchPacket(grant_type, username, password : string) : string;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject:= TJSONObject.Create;
    try
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_USERNAME,username);
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_PASSWORD,password);

        sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
function ParseAutchPacket(AJSONValue: TJSONValue) : boolean;
var
    Enums: TJSONPairEnumerator;
    userInfo : TUserInfo;
    FoundIndex : Integer;
    sJson : string;
    tempJson : TJSONObject;
begin
  try
    if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
    begin
        Result := false;
        Exit;
    end;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;

      try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_AUTCH, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
          begin
                case FoundIndex of
                  0:
                  begin
                    uGlobal.Token.Value := Enums.Current.JsonValue.Value.Trim;
                    Continue;
                  end;
                else
                    Continue;
                end;
          end
          else
            Continue;
        end;
      finally
            Enums.Free;
      end;
    Result := uGlobal.Token.isExists;
  except
    Result := False;
  end;
end;
{$ENDREGION}
{------------------------------------------------------------------------------}
{$REGION 'Создание и парсинг запроса авторизации через соцсеть'}
function CreateExtraAutchPacket(socialnetwork, token : string) : string;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject:= TJSONObject.Create;
    try
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_SOCIALNETWORK, socialnetwork);
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_TOKEN,token);

        sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
{function ParseExtraAutchPacket(AJSONValue: TJSONValue) : boolean;
var
    Enums: TJSONPairEnumerator;
    userInfo : TUserInfo;
    FoundIndex : Integer;
    sJson : string;
    res : Boolean;
    tempJson : TJSONObject;
begin
  try
    if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
    begin
        Result := false;
        Exit;
    end;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;

      try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_AUTCH, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
                case FoundIndex of
                  0: uGlobal.Token.Value := Enums.Current.JsonValue.Value.Trim;
                  3:
                   begin
                        uGlobal.SetCurrentUser(nil);
                        uGlobal.CurrentUser.FUserName := Enums.Current.JsonValue.Value.Trim;
                   end;
                end;
        end;
      finally
            Enums.Free;
      end;

    res := uGlobal.Token.isExists and  Assigned(uGlobal.CurrentUser);
    Result := res;
  except
    Result := False;
  end;
end;
{$ENDREGION}
{------------------------------------------------------------------------------}
{$REGION 'Создание и парсинг запроса регистрации'}
function CreateRegisterPacket(login, password : string; gender, socialstatus : integer) : string;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject:= TJSONObject.Create;
    try
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_USERNAME,login);
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_PASSWORD,password);
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_GENDER,IntToStr(gender));
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_SOCSTATUS,IntToStr(socialstatus));
        sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
{function ParseRegisterPacket(AJSONValue: TJSONValue) : boolean;
var
    Enums: TJSONPairEnumerator;
    userInfo : TUserInfo;
    FoundIndex : Integer;
    sJson : string;
    tempJson : TJSONObject;
begin
  try
    if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
    begin
        Result := false;
        Exit;
    end;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;

      try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_AUTCH, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
                case FoundIndex of
                  0: uGlobal.CurrentToken := Enums.Current.JsonValue.Value.Trim;
                  3:
                   begin
                        uGlobal.SetCurrentUser(nil);
                        uGlobal.CurrentUser.FUserName := Enums.Current.JsonValue.Value.Trim;
                   end;
                end;
        end;
      finally
            Enums.Free;
      end;
    Result := not uGlobal.CurrentToken.IsEmpty;
  except
    Result := False;
  end;
end;
{$ENDREGION}
{------------------------------------------------------------------------------}
{$REGION 'Парсинг запроса инфы пользователя'}
/// Парсинг информации пользователя
function ParseUserItem(AJSONValue: TJSONValue; const pUser : Pointer) : boolean;
var
    Enums: TJSONPairEnumerator;
    user : TUserInfo;
    FoundIndex : Integer;
    sJson, stemp, fikeJson : string;
    res : Boolean;
    tempJson : TJSONObject;
    quests : TList<TQuest>;
begin
  Result := false;
  try
    if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
        Exit;
    user := TUserInfo(pUser);
    if not Assigned(user) then
        Exit(False);
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;
    try
        while Enums.MoveNext do
        begin
          try
              stemp := Enums.Current.JsonString.Value.Trim;
              if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_USERINFO, stemp, FoundIndex, TStringComparer.Ordinal) then
              begin
                    case FoundIndex of
                     0: begin
                            user.FBar := ParseBarItem(Enums.Current.JsonValue);
                            continue;
                        end;
                     1: begin
                            uUtils.UtcToNative(user.FBirthDate, Enums.Current.JsonValue.Value);
                            continue;
                        end;
                     2:
                        begin
                            ParseDictonaryItem(user.FCity, Enums.Current.JsonValue);
                            continue;
                        end;
                     3:
                        begin
                            ParseDictonaryItem(user.FCountry, Enums.Current.JsonValue);
                            continue;
                        end;
                     4:
                        begin
                            user.FCurrentAchivePoint := Enums.Current.JsonValue.Value.Trim;
                            continue;
                        end;
                     5:
                        begin
                            user.Email := Enums.Current.JsonValue.Value.Trim;
                            continue;
                        end;
                     6:
                        begin
                            user.FirstName := Enums.Current.JsonValue.Value.Trim;
                            continue;
                        end;
                     7:
                        begin
                            ParseDictonaryItem(user.FGender, Enums.Current.JsonValue);
                            continue;
                        end;
                     8:
                        begin
                            user.LastName := Enums.Current.JsonValue.Value.Trim;
                            continue;
                        end;
                     9:
                        begin
                            user.MiddleName := Enums.Current.JsonValue.Value.Trim;
                            continue;
                        end;
                     10:
                        begin
                             user.Phone := Enums.Current.JsonValue.Value.Trim;
                             continue;
                         end;
                     11:
                        begin
                            ParseDictonaryItem(user.FRelationship, Enums.Current.JsonValue);
                            continue;
                        end;
                     12:
                        begin
                            ParseDictonaryItem(user.FSocialStatus, Enums.Current.JsonValue);
                            continue;
                        end;
                     13:
                        begin
                            if not uUtils.ValidateJSONObject(Enums.Current.JsonValue, fikeJson) then
                            begin
                                //uGlobal.GetBitmap(fikeJson, user.FAvatar.Bitmap, 0, 0);
                                user.FAvatar.URL := fikeJson;
                            end;
                            continue;
                        end;
                     14:
                        begin
                            quests := TList<TQuest>.Create;
                            ParseQuest(quests, Enums.Current.JsonValue);
                            if quests.Count > 0 then
                                user.FQuests := quests;
                            continue;
                         end;
                     15:
                         begin
                            user.FId := Enums.Current.JsonValue.Value.Trim;
                            continue;
                         end;
                     16: begin
                             user.FLevel := Enums.Current.JsonValue.Value.Trim;
                             continue;
                         end;
                     else
                         begin
                            continue;
                         end;
                    end;
              end
              else
                Continue;
          except
            on e : Exception do
            begin
                uGlobal.CurrentErrorMsg := e.Message;
            end;
          end;
        end;
        //TEMP
        if user.FAvatar.URL.IsEmpty then
            user.FAvatar.URL := user.FId + '.jpg';
    finally
        Enums.Free;
    end;
    Result := true;
  except
    on e : Exception do
        uGlobal.CurrentErrorMsg := e.Message;
  end;
end;
{------------------------------------------------------------------------------}
// Парсинг справочников (города, гендер и пр)
function ParseDictonaryItem(var dictonaryItem : TDictonaryItem; AJSONValue: TJSONValue) : boolean;
var
    Enums: TJSONPairEnumerator;
    tempJson : TJSONObject;
    userInfo : TUserInfo;
    FoundIndex : Integer;
    sJson : string;
begin
  try
    if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
    begin
        Result := false;
        Exit;
    end;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;
      try
        while Enums.MoveNext do
        begin
            try
              if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_DICTONARY_ITEM, Enums.Current.JsonString.Value, FoundIndex,
                    TStringComparer.Ordinal) then
              begin
                    case FoundIndex of
                      0,1:
                        begin
                            dictonaryItem.FId := StrToInt(Enums.Current.JsonValue.Value.Trim);
                            continue;
                        end;
                      2,3:
                        begin
                            dictonaryItem.FName := Enums.Current.JsonValue.Value.Trim;
                            continue;
                        end;
                      else
                        Continue;
                    end;
              end
              else
                Continue;
            except
                on e : Exception do
                begin
                    CurrentErrorMsg := e.Message;
                end;
            end;
        end;
      finally
            Enums.Free;
      end;
    Result := True
  except
    on e : Exception do
    begin
        uGlobal.CurrentErrorMsg := e.Message;
        Result := false;
    end;
  end;
end;
{------------------------------------------------------------------------------}
/// Парсинг Списка баров
function ParseBars(var bars : TList<TBar>; AJSONValue: TJSONValue) : boolean;
var
    Enums: TJSONArrayEnumerator;
    tempJson : TJSONArray;
    FoundIndex : Integer;
    sJson, stemp : string;
    bar : TBar;
begin
    if not Assigned(bars) then
    begin
        bars := TList<TBar>.Create;
    end;
    bars.Clear;
    try
        if uUtils.ValidateJSONArray(AJSONValue, tempJson) then
        begin
            Result := false;
            Exit;
        end;
        Enums:=tempJson.GetEnumerator;
        sJson := tempJson.ToJSON;
        try
            while Enums.MoveNext do
            begin        //Enums.Current.Null
                stemp := Enums.Current.ToJSON;
                bar := ParseBarItem(Enums.Current);
                bars.Add(bar);
            end;
        finally
            Enums.Free;
        end;
        Result := True
    except
        on e : Exception do
        begin
            uGlobal.CurrentErrorMsg := e.Message;
            Result := false;
        end;
    end;
end;
{------------------------------------------------------------------------------}
/// Парсинг информации по бару
function ParseBarItem(AJSONValue: TJSONValue) : TBar;
var
    Enums: TJSONPairEnumerator;
    tempJson, fikeJson : TJSONObject;
    FoundIndex : Integer;
    sJson : string;
    bar : TBar;
begin
    bar := TBar.Create;
    try
        if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
        begin
            Result := bar;
            Exit;
        end;
        Enums:=tempJson.GetEnumerator;
        sJson := tempJson.ToJSON;
        try
            while Enums.MoveNext do
            begin
              if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_BAR, Enums.Current.JsonString.Value, FoundIndex,
                    TStringComparer.Ordinal) then
              begin
                    case FoundIndex of
                      0:
                      begin
                        ParseBarDescription(bar.FDescription, Enums.Current);
                        Continue;
                      end;
                      1:
                      begin
                        bar.FId := StrToInt(Enums.Current.JsonValue.Value.Trim);
                        Continue;
                      end;
                      3:
                      begin
                        bar.FName := Enums.Current.JsonValue.Value.Trim;
                        Continue;
                      end;
                      4:
                      begin
                        bar.FRemoved := StrToBool(Enums.Current.JsonValue.Value.Trim);
                        Continue;
                      end;
                      else
                        Continue;
                    end;
              end
              else
                Continue;
            end;
        finally
            Enums.Free;
        end;
    except
        on e : Exception do
        begin
            uGlobal.CurrentErrorMsg := e.Message;
        end;
    end;
    Result := bar;
end;
{------------------------------------------------------------------------------}
/// Парсинг описания бара
function ParseBarDescription(var barDescription : TBarDescription; AJSONObject: TJSONPair) : boolean;
var
    Enums: TJSONPairEnumerator;
    tempJson : TJSONObject;
    FoundIndex : Integer;
    sJson : string;
begin
  try
    if uUtils.ValidateJSONObject(AJSONObject.JsonValue, tempJson) then
    begin
        Result := false;
        Exit;
    end;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;

      try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_BAR_DESC, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
          begin
                case FoundIndex of
                  0:
                  begin
                    barDescription.FAdress := Enums.Current.JsonValue.Value.Trim;
                    Continue;
                  end;
                  1:
                  begin
                    ParseLogo(barDescription.Logo, Enums.Current.JsonValue);
                    Continue;
                  end;
                  2:
                  begin
                    ParseDictonaryItem(barDescription.FCity, Enums.Current.JsonValue);
                    Continue;
                  end;
                  3:
                  begin
                    barDescription.FPhone := Enums.Current.JsonValue.Value.Trim;
                    Continue;
                  end;
                  4:
                  begin
                    barDescription.FShortDescription := Enums.Current.JsonValue.Value.Trim;
                    Continue;
                  end;
                  else
                    Continue;
                end;
          end
          else
            Continue;
        end;
      finally
            Enums.Free;
      end;
    Result := True
  except
    on e : Exception do
    begin
        uGlobal.CurrentErrorMsg := e.Message;
        Result := false;
    end;
  end;
end;
{------------------------------------------------------------------------------}
/// Парсинг Списка квестов
function ParseQuest(var quests : TList<TQuest>; AJSONValue: TJSONValue) : boolean;
var
    Enums: TJSONArrayEnumerator;
    tempJson : TJSONArray;
    FoundIndex : Integer;
    sJson, stemp : string;
    quest : TQuest;
begin
    if not Assigned(quests) then
    begin
        quests := TList<TQuest>.Create;
    end;
    quests.Clear;
    try

        if uUtils.ValidateJSONArray(AJSONValue, tempJson) then
        begin
            Result := false;
            Exit;
        end;
        Enums:=tempJson.GetEnumerator;

        try
            while Enums.MoveNext do
            begin
                stemp := Enums.Current.ToJSON;
                quest := ParseQuestItem(Enums.Current);
                quests.Add(quest);
                quest.FNumber := quests.Count - 1;
            end;
        finally
            Enums.Free;
        end;
        Result := True
    except
        on e : Exception do
        begin
            uGlobal.CurrentErrorMsg := e.Message;
            Result := false;
        end;
    end;
end;
{------------------------------------------------------------------------------}
/// Парсинг квеста
function ParseQuestItem(AJSONValue: TJSONValue) : TQuest;
var
    Enums: TJSONPairEnumerator;
    tempJson : TJSONObject;
    FoundIndex : Integer;
    sJson : string;
    quest : TQuest;
begin
  quest := TQuest.Create;
  try
    if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
    begin
        Result := quest;
        Exit;
    end;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;

      try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_QUEST, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
          begin
                case FoundIndex of
                  0:
                  begin
                      uUtils.UtcToNative(quest.FExpirationDate, Enums.Current.JsonValue.Value);
                      Continue;
                  end;
                  1:
                  begin
                      quest.FId := StrToInt(Enums.Current.JsonValue.Value.Trim);
                      Continue;
                  end;
                  2:
                  begin
                    ParseQuestInfo(quest.FQuestInfo, Enums.Current);
                    Continue;
                  end;
                  3, 5:
                  begin
                    ParseDictonaryItem(quest.FQuestStatus, Enums.Current.JsonValue);
                    Continue;
                  end;
                  4:
                  begin
                    uUtils.UtcToNative(quest.FStartDate, Enums.Current.JsonValue.Value);
                    Continue;
                  end
                  else
                    Continue;
                end;
          end
          else
            Continue;
        end;
      finally
            Enums.Free;
      end;
  except
    on e : Exception do
    begin
        uGlobal.CurrentErrorMsg := e.Message;
    end;
  end;
  Result := quest;
end;
{------------------------------------------------------------------------------}
/// Парсинг информации по квесту
function ParseQuestInfo(var questInfo : TQuestInfo; AJSONObject: TJSONPair) : boolean;
var
    Enums: TJSONPairEnumerator;
    tempJson, fikeJson : TJSONObject;
    FoundIndex : Integer;
    sJson : string;
begin
  try
    if uUtils.ValidateJSONObject(AJSONObject.JsonValue, tempJson) then
    begin
        Result := false;
        Exit;
    end;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;

      try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_QUEST_INFO, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
          begin
                case FoundIndex of
                  0:
                  begin
                    ParseBars(questInfo.FBars, Enums.Current.JsonValue);
                    Continue;
                  end;
                  2:
                  begin
                    questInfo.FExperience := StrToInt(Enums.Current.JsonValue.Value.Trim);
                    Continue;
                  end;
                  3:
                  begin
                    questInfo.FId := StrToInt(Enums.Current.JsonValue.Value.Trim);
                    Continue;
                  end;
                  4:
                  begin
                    questInfo.FName := Enums.Current.JsonValue.Value.Trim;
                    Continue;
                  end;
                  5:
                  begin
                    ParseQuestDescription(questInfo.FQuestDescription, Enums.Current);
                    Continue;
                  end;
                  6:
                  begin
                    ParseLogo(questInfo.Logo, Enums.Current.JsonValue);
                    Continue;
                  end;
                  7:
                  begin
                    ParseDictonaryItem(questInfo.FQuestStatus, Enums.Current.JsonValue);
                    Continue;
                  end;
                  8:
                  begin
                    ParseDictonaryItem(questInfo.FRarity, Enums.Current.JsonValue);
                    Continue;
                  end;
                  10:
                  begin
                    ParseDictonaryItem(questInfo.FUserLevel, Enums.Current.JsonValue);
                    Continue;
                  end;
                  else
                    Continue;
                end;
          end
          else
            Continue;
        end;
      finally
            Enums.Free;
      end;
      Result := True;
  except
    on e : Exception do
    begin
        uGlobal.CurrentErrorMsg := e.Message;
        Result := False;
    end;
  end;
end;
{------------------------------------------------------------------------------}
/// Парсинг описания квеста
function ParseQuestDescription(var questDescription : TQuestDescription; AJSONObject: TJSONPair) : boolean;
var
    Enums: TJSONPairEnumerator;
    tempJson : TJSONObject;
    FoundIndex : Integer;
    sJson : string;
begin
  try
    if uUtils.ValidateJSONObject(AJSONObject.JsonValue, tempJson) then
    begin
        Result := false;
        Exit;
    end;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;

      try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_QUEST_DESC, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
          begin
                case FoundIndex of
                  0:
                  begin
                    questDescription.Description := Enums.Current.JsonValue.Value.Trim;
                    Continue;
                  end;
                  1:
                  begin
                    questDescription.Condition := Enums.Current.JsonValue.Value.Trim;
                    Continue;
                  end;
                  3:
                  begin
                    questDescription.Reward := Enums.Current.JsonValue.Value.Trim;
                    Continue;
                  end;
                  else
                    Continue;
                end;
          end
          else
            Continue;
        end;
      finally
            Enums.Free;
      end;
      Result := True;
  except
    on e : Exception do
    begin
        uGlobal.CurrentErrorMsg := e.Message;
        Result := False;
    end;
  end;
end;
{$ENDREGION}
{------------------------------------------------------------------------------}
/// Создание запроса на редактирование профиля
function CreateProfileEditPacket(profileEdit : TProfileEdit) : string;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject:= TJSONObject.Create;
    try
         AJSONObject.AddPair(profileEdit.FConct.FUserId, profileEdit.FUserId);
         AJSONObject.AddPair(profileEdit.FConct.FFirtsName, profileEdit.FFirtsName);
         AJSONObject.AddPair(profileEdit.FConct.FLastName, profileEdit.FLastName);
         AJSONObject.AddPair(profileEdit.FConct.FBirthDate, profileEdit.FBirthDate);
         AJSONObject.AddPair(profileEdit.FConct.FPhone, profileEdit.FPhone);
         AJSONObject.AddPair(profileEdit.FConct.FSotialStatusId,
            Integer.ToString(profileEdit.FSotialStatus.FId));
         AJSONObject.AddPair(profileEdit.FConct.FRelationshipId,
            Integer.ToString(profileEdit.FRelationship.FId));
         AJSONObject.AddPair(profileEdit.FConct.FEmail, profileEdit.FEmail);
         {AJSONObject.AddPair(profileEdit.FConct.FCountryId,
            Integer.ToString(profileEdit.FCountry.FId)); }
         AJSONObject.AddPair(profileEdit.FConct.FCityId,
            Integer.ToString(profileEdit.FCity.FId));
         AJSONObject.AddPair(profileEdit.FConct.FGenderId,
            Integer.ToString(profileEdit.FGender.FId));
        sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
/// Создание запроса на восстановления пароля
function CreateResendPassword(email, guid, text : string) : string;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject:= TJSONObject.Create;
    try
         AJSONObject.AddPair(uConst.TApiConst.TParam.S_EMAIL, email);
         AJSONObject.AddPair(uConst.TApiConst.TParam.S_GUID, guid);
         AJSONObject.AddPair(uConst.TApiConst.TParam.S_TEXT, text);
        sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
/// Создание запроса на загрузку аватара пользователя
function CreateUploadAvatarPacket(image, filename : string) : string;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject:= TJSONObject.Create;
    try
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_FILENAME, filename);
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_IMAGE, image);
        sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
/// Парсинг справочника уровней
function ParseGetAllLevelsPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseGetDictionaryPacket(AJSONValue, uGlobal.FDictionary.FLevels, false);
end;
{------------------------------------------------------------------------------}
/// Парсинг справочника городов
function ParseGetAllCitiesPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseGetDictionaryPacket(AJSONValue, uGlobal.FDictionary.FCities);
end;
{------------------------------------------------------------------------------}
/// Парсинг справочника городов
function ParseGetAllSocialStatusesPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseGetDictionaryPacket(AJSONValue, uGlobal.FDictionary.FSocialStatuses);
end;
{------------------------------------------------------------------------------}
/// Парсинг справочника городов
function ParseGetAllRelationshipsPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseGetDictionaryPacket(AJSONValue, uGlobal.FDictionary.FRelationships);
end;
{------------------------------------------------------------------------------}
/// Парсинг справочника тип акции
function ParseGetAllRaritiesPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseGetDictionaryPacket(AJSONValue, uGlobal.FDictionary.FRarities);
end;
{------------------------------------------------------------------------------}
/// Парсинг справочника статуса квеста
function ParseGetAllQuestStatusesPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseGetDictionaryPacket(AJSONValue, uGlobal.FDictionary.FQuestStatuses);
end;
{------------------------------------------------------------------------------}
// Парсинг справочника
function ParseGetDictionaryPacket(AJSONValue: TJSONValue; var aList : TDictionaryList;//TList<TDictonaryItem>;
    isSort : boolean) : boolean;
var
    Enums: TJSONArrayEnumerator;
    tempJson : TJSONArray;
    stemp : string;
    JSON: TJSONValue;
    item : TDictonaryItem;
    Comparison: TComparison<TDictonaryItem>;
begin
    if not Assigned(aList) then
        aList := TDictionaryList.Create//TList<TDictonaryItem>.Create
    else
        aList.Clear;
    try
        if uUtils.ValidateJSONArray(AJSONValue, tempJson) then
        begin
            Result := false;
            Exit;
        end;
        Enums := tempJson.GetEnumerator;
        try
            while Enums.MoveNext do
            begin
                stemp := Enums.Current.ToJSON;
                ParseDictonaryItem (item, Enums.Current);
                aList.Add(item);
            end;
        finally
            Enums.Free;
        end;
        if isSort then
        begin
            Comparison :=
                function(const Left, Right: TDictonaryItem): Integer
                begin
                    Result := CompareText(Left.FName, Right.FName);
                end;
            aList.Sort(TComparer<TDictonaryItem>.Construct(Comparison));
        end;
        Result := True;
        uUtils.ExecuteNotifyEvent(aList.OnLoading);
    except
        on e : Exception do
        begin
            uGlobal.CurrentErrorMsg := e.Message;
            Result := false;
        end;
    end;
end;
{------------------------------------------------------------------------------}
/// Создание запроса на отмену активной акции пользователя
function CreateCancelActiveQuestPacket(UserActiveQuestId : integer) : string;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject:= TJSONObject.Create;
    try
         AJSONObject.AddPair(uConst.TApiConst.TParam.S_ID, IntToStr(UserActiveQuestId));
         sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
/// Парсинг Активных квестов
function ParseActiveQuestPacket(AJSONValue: TJSONValue) : boolean;
var
    quests : TList<TQuest>;
begin
    quests := nil;
    if ParseQuest(quests, AJSONValue) then
    begin
        uGlobal.CurrentUser.FQuests := quests;
        uUtils.ExecuteNotifyEvent(uGlobal.CurrentUser.OnChangeActiveQuests);
    end;
end;
{------------------------------------------------------------------------------}
/// Парсинг выполненных квестов
function ParseCompletedQuestsPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseCompletedQuests(uGlobal.CurrentUser.FQuestsCompleted, AJSONValue);
    if Result then
    begin
        uUtils.ExecuteNotifyEvent(uGlobal.CurrentUser.OnChangeCompletedQuests);
        uUtils.ExecuteNotifyEvent(uGlobal.CurrentUser.OnCHangeHistory);
    end;
end;
{------------------------------------------------------------------------------}
/// Парсинг выполненных квестов
function ParseCompletedQuests(var completedQuests : TList<TQuest>; AJSONValue: TJSONValue) : boolean;
var
    Enums: TJSONArrayEnumerator;
    tempJson : TJSONArray;
    FoundIndex : Integer;
    sJson, stemp : string;
    completedQuest : TQuest;
begin
    if not Assigned(completedQuests) then
        completedQuests := TList<TQuest>.Create
    else
        completedQuests.Clear;
    try
        if uUtils.ValidateJSONArray(AJSONValue, tempJson) then
            exit(True);
        Enums:=tempJson.GetEnumerator;
        sJson := tempJson.ToJSON;
        try
            while Enums.MoveNext do
            begin
                stemp := Enums.Current.ToJSON;
                completedQuest := ParseCompletedQuestItem(Enums.Current);
                completedQuests.Add(completedQuest);
            end;
        finally
            Enums.Free;
        end;
        completedQuests.Reverse;
        Result := True
    except
        on e : Exception do
        begin
            uGlobal.CurrentErrorMsg := e.Message;
            Result := false;
        end;
    end;
end;
{------------------------------------------------------------------------------}
/// Парсинг выполненного квеста
function ParseCompletedQuestItem(AJSONValue: TJSONValue) : TQuest;
var
    Enums: TJSONPairEnumerator;
    tempJson : TJSONObject;
    FoundIndex : Integer;
    sJson, fikeJson : string;
    completedQuest : TQuest;
begin
    completedQuest := TQuest.Create;
  try
    if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
    begin
        Result := completedQuest;
        Exit;
    end;
    Enums := tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;
    try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_QUEST_COMPLETED, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
          begin
                case FoundIndex of
                  0:
                  begin
                    completedQuest.FQuestInfo.FBars.Add(ParseBarItem(Enums.Current.JsonValue));
                    Continue;
                  end;
                  1:
                  begin
                    uUtils.UtcToNative(completedQuest.FCompleteTime, Enums.Current.JsonValue.Value);
                    Continue;
                  end;
                  2:
                  begin
                    completedQuest.FQuestInfo.FName := Enums.Current.JsonValue.Value.Trim;
                    Continue;
                  end;
                  3:
                  begin
                    completedQuest.FQuestInfo.FExperience := StrToInt(Enums.Current.JsonValue.Value.Trim);
                    Continue;
                  end;
                  4:
                  begin
                    if not uUtils.ValidateJSONObject(Enums.Current.JsonValue, fikeJson) then
                    begin
                        completedQuest.FQuestInfo.Logo.URL := fikeJson;
                    end;
                    Continue;
                  end;
                  5:
                  begin
                    completedQuest.FId := StrToInt(Enums.Current.JsonValue.Value.Trim);
                    Continue;
                  end;
                  else
                    Continue;
                end;
          end
          else
            Continue;
        end;
    finally
        Enums.Free;
    end;
  except
    on e : Exception do
    begin
        uGlobal.CurrentErrorMsg := e.Message;
    end;
  end;
  Result := completedQuest;
end;
{------------------------------------------------------------------------------}
/// Парсинг логотипа/картинки
function ParseLogo(plogo :Pointer; AJSONValue: TJSONValue) : boolean;
var
    Enums: TJSONPairEnumerator;
    tempJson : TJSONObject;
    FoundIndex : Integer;
    sJson, tempURL : string;
    logo : TLogo;
begin
  try
    if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
    begin
        Result := false;
        Exit;
    end;
    logo := TLogo(plogo);
    if not Assigned(logo) then
        exit;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;
      try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_LOGO, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
          begin
                case FoundIndex of
                  0,1,2,3,4 :
                  begin
                    if not uUtils.ValidateJSONObject(Enums.Current.JsonValue, tempURL) then
                    begin
                        //uGlobal.GetBitmap(tempURL, logo.Bitmap, 0, 0);
                        logo.URL := tempURL;
                    end;
                    Continue;
                  end;
                  else
                    Continue;
                end;
          end
          else
            Continue;
        end;
      finally
            Enums.Free;
      end;
    Result := True
  except
    on e : Exception do
    begin
        uGlobal.CurrentErrorMsg := e.Message;
        Result := false;
    end;
  end;
end;
{------------------------------------------------------------------------------}
/// Создание запроса на оставление отзыва бара
function CreateBarSendCommentPacket(comment : TComment) : string;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject:= TJSONObject.Create;
    try
         AJSONObject.AddPair(uConst.TApiConst.TParam.S_BAR_ID, IntToStr(comment.FBarId));
         AJSONObject.AddPair(uConst.TApiConst.TParam.S_TEXT, comment.FText);
         sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
/// Парсинг отзывов по бару
function ParseBarCommentsPacket(AJSONValue: TJSONValue) : boolean;
var
    Enums: TJSONArrayEnumerator;
    tempJson : TJSONArray;
    sJson, stemp : string;
    comments : TList<TComment>;
    comment : TComment;
begin
    comments := TList<TComment>.Create;
    try
        if uUtils.ValidateJSONArray(AJSONValue, tempJson) then
            exit(True);
        sJson := tempJson.Value;
        Enums := tempJson.GetEnumerator;
        try
            while Enums.MoveNext do
            begin
                stemp := Enums.Current.ToJSON;
                comment := ParseCommentItem(Enums.Current);
                comments.Add(comment);
            end;
        finally
            Enums.Free;
        end;
        comments.Reverse;
        Result := True
    except
        on e : Exception do
        begin
            uGlobal.CurrentErrorMsg := e.Message;
            Result := false;
        end;
    end;
    uGlobal.FCurrentBarComment := comments;
end;
{------------------------------------------------------------------------------}
/// Парсинг отзыва по бару
function ParseCommentItem(AJSONValue: TJSONValue) : TComment;
var
    Enums: TJSONPairEnumerator;
    tempJson : TJSONObject;
    FoundIndex : Integer;
    sJson : string;
    comment : TComment;
begin
  try
    if uUtils.ValidateJSONObject(AJSONValue, tempJson) then
    begin
        Result := comment;
        Exit;
    end;
    Enums:=tempJson.GetEnumerator;
    sJson := tempJson.ToJSON;
      try
        while Enums.MoveNext do
        begin
          if TArray.BinarySearch<String>(uConst.ARRAY_PARSE_BAR_COMMENTS, Enums.Current.JsonString.Value, FoundIndex,
                TStringComparer.Ordinal) then
                case FoundIndex of
                  0: comment.FBarId := StrToInt(Enums.Current.JsonValue.Value.Trim);
                  1: uUtils.UtcToNative(comment.FDate, Enums.Current.JsonValue.Value);
                  2: comment.FId := StrToInt(Enums.Current.JsonValue.Value.Trim);
                  3: comment.FStatusId := StrToInt(Enums.Current.JsonValue.Value.Trim);
                  4: comment.FText := Enums.Current.JsonValue.Value.Trim;
                  5: comment.User.FAvatar.URL := Enums.Current.JsonValue.Value.Trim;
                  6: comment.User.FId := Enums.Current.JsonValue.Value.Trim;
                end;
        end;
      finally
            Enums.Free;
      end;
    Result := comment
  except
    on e : Exception do
    begin
        uGlobal.CurrentErrorMsg := e.Message;
        Result := comment;
    end;
  end;
end;
{------------------------------------------------------------------------------}
/// Парсинг друзей
function ParseFriendsPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseUsers(AJSONValue, uGlobal.CurrentUser.FFreinds);
end;
{------------------------------------------------------------------------------}
/// Парсинг друзей
function ParseUsers(AJSONValue: TJSONValue; var list : TList<TUserInfo>) : boolean;
var
    Enums: TJSONArrayEnumerator;
    tempJson : TJSONArray;
    sJson, stemp : string;
    user : TUserInfo;
begin
    Result := false;
    try
        if not Assigned(list) then
            list := TList<TUserInfo>.Create
        else
            list.Clear;
        if uUtils.ValidateJSONArray(AJSONValue, tempJson) then
            exit(True);
        sJson := tempJson.Value;
        Enums := tempJson.GetEnumerator;
        try
            while Enums.MoveNext do
            begin
                user := TUserInfo.Create;
                ParseUserItem(Enums.Current, user);
                list.Add(user);
            end;
        finally
            Enums.Free;
        end;
        Result := True
    except
        on e : Exception do
            uGlobal.CurrentErrorMsg := e.Message;
    end;
end;
{------------------------------------------------------------------------------}
/// Парсинг пользователей при поиске по логину
function ParseUserListPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseUsers(AJSONValue, uGlobal.CurrentUserSearch);
end;
{------------------------------------------------------------------------------}
/// Парсинг информации текущего профиля
function ParseProfilePacket(AJSONValue: TJSONValue) : boolean;
begin
    {FreeAndNil(uGlobal.CurrentUser);
    uGlobal.CurrentUser := TUserInfo.Create;}
    if not Assigned(uGlobal.CurrentUser) then
        uGlobal.CurrentUser := TUserInfo.Create;
    Result := ParseUserItem(AJSONValue, uGlobal.CurrentUser) and
        uGlobal.Token.isExists;
end;
{------------------------------------------------------------------------------}
/// Парсинг подписчиков
function ParseFollowersPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseUsers(AJSONValue, uGlobal.CurrentUser.FFollowers);
end;
{------------------------------------------------------------------------------}
/// Парсинг приглашенных друзей (не подтвердили)
function ParseFriendsInvitedPacket(AJSONValue: TJSONValue) : boolean;
begin
    Result := ParseUsers(AJSONValue, uGlobal.CurrentUser.FFreindsInvited);
end;
{------------------------------------------------------------------------------}
/// Парсинг капчи
function ParseCaptcha(AJSONValue: TJSONValue) : boolean;
var
    temp :  string;
    bmp : TBitmap;
begin
    uUtils.SynchExecute(
        procedure
        begin
            temp := AJSONValue.ToString;
            //Result := string.IsNullOrEmpty(temp);
            bmp := TBitmap.Create;
            bmp.Base64 := temp;
            FreeAndNil(uGlobal.CurrentCaptcha);
            uGlobal.CurrentCaptcha := bmp;
        end
    );
    Result := True and not uGlobal.CurrentCaptcha.IsEmpty;
end;
{------------------------------------------------------------------------------}
function ParseCaptchaName(strings : TStrings) : Boolean;
begin
    uGlobal.CurrentCaptchaName :=
        strings.Values[uConst.TApiConst.TAuthorize.S_CAPTHA];
end;
{------------------------------------------------------------------------------}
/// Создание запроса на получения qr-coda
function CreateQrCode(userId, questId : string; deviceType : string = '';
    token : string = ''; androidDeviceid : string = '';
    iosSertPassw : string = '') : string;
procedure AddPairNotEMpty(var AJSONObject: TJSONObject; paramName, paramValue : string);
begin
    if  not paramValue.IsEmpty then
        AJSONObject.AddPair(paramName, paramValue);
end;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject := TJSONObject.Create;
    try
         AJSONObject.AddPair(uConst.TApiConst.TParam.S_USER_ID, userId);
         AJSONObject.AddPair(uConst.TApiConst.TParam.S_QUEST_ID, questId);
         AddPairNotEMpty(AJSONObject, uConst.TApiConst.TParam.S_DEVICE_TYPE, deviceType);
         AddPairNotEMpty(AJSONObject, uConst.TApiConst.TParam.S_TOKEN, token);
         AddPairNotEMpty(AJSONObject, uConst.TApiConst.TParam.S_ANDROID_DEVICE_ID, androidDeviceid);
         AddPairNotEMpty(AJSONObject, uConst.TApiConst.TParam.S_IOS_SERT_PASSW, iosSertPassw);
         sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
/// Парсинг изменений пользователя
function ParseChangeActions(AJSONValue: TJSONValue) : boolean;
begin
    uGlobal.CurrentUser.ChangeList.Clear;
    Result := ParseListItem(AJSONValue,
        procedure (Value : TJSONValue)
        var
            changeItem : uClasses.TUserInfo.TChangeItem;
            temp : string;
        begin
            Value.TryGetValue<Integer>(uClasses.TUserInfo.TChangeItem.S_ID, changeItem.FId);
            if Value.TryGetValue<string>(uClasses.TUserInfo.TChangeItem.S_TIME, temp) then
                uUtils.UtcToNative(changeItem.FTime, temp);
            uGlobal.CurrentUser.ChangeList.Add(changeItem);
        end);
    if Result then
        uUtils.ExecuteNotifyEvent(uGlobal.CurrentUser.OnChangeList);
end;
{------------------------------------------------------------------------------}
function ParseChangeTime(strings : TStrings) : Boolean;
begin
    if not Assigned(uGlobal.CurrentUser) then
        exit;
    uGlobal.CurrentUser.LastUpdate := uGlobal.FServerTime;
end;
{------------------------------------------------------------------------------}
/// Парсинг врмени сервера
function ParseServerTime(strings : TStrings) : Boolean;
var
    date : string;
    fmt : TFormatSettings;
    dt, aNow : TDateTime;
begin
    date := strings.Values[uConst.TApiConst.S_SERVER_TIME];
    aNow := Now;
    if not date.IsEmpty then
    begin
        fmt.ShortDateFormat:='dd.mm.yyyy';
        fmt.DateSeparator  :='.';
        fmt.LongTimeFormat :='hh:nn:ss';
        fmt.TimeSeparator  :=':';
        uGlobal.FServerTime := StrToDateTime(date, fmt);
    end
    else
        uGlobal.FServerTime := aNow;
    uGlobal.FLocalTime := aNow;
    uGlobal.FLoaclInc := MillisecondsBetween(aNow, uGlobal.FServerTime);
end;
{------------------------------------------------------------------------------}
/// Парсинг массива и запись в лист
function ParseListItem(AJSONValue: TJSONValue; parse : TProc<TJSONValue>) : boolean;
var
    Enums: TJSONArrayEnumerator;
    tempJson : TJSONArray;
    sJson, stemp : string;
begin
    Result := false;
    try
        if not Assigned(parse) then
            Exit;
        if uUtils.ValidateJSONArray(AJSONValue, tempJson) then
            exit(True);
        sJson := tempJson.ToJSON;
        Enums := tempJson.GetEnumerator;
        try
            while Enums.MoveNext do
            begin
                parse(Enums.Current);
            end;
        finally
            Enums.Free;
        end;
        Result := True
    except
        on e : Exception do
            uGlobal.CurrentErrorMsg := e.Message;
    end;
end;
{------------------------------------------------------------------------------}
/// Создание запроса на CheckIn
function CreateCheckIn(barId : string) : string;
var
    AJSONObject: TJSONObject;
    sJson : string;
begin
    AJSONObject:= TJSONObject.Create;
    try
        AJSONObject.AddPair(uConst.TApiConst.TParam.S_BAR_ID, barId);
        sJson := AJSONObject.ToJSON;
    finally
        AJSONObject.Free;
    end;
    Result := sJson;
end;
{------------------------------------------------------------------------------}
end.
