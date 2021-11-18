---
title: Работа с файлами формата ELF из Хаскела
date: November 17, 2021
---

Работа с файлами формата ELF -- популярная тема на Хабре.
(["Введение в ELF-файлы в Linux: понимание и анализ"](https://habr.com/ru/post/480642/),
["Минимизация файла ELF – попробуем в 2021?"](https://habr.com/ru/company/ruvds/blog/583576/) и т. д.)

Существуют библиотеки для Хаскела для работы с этими файлами: `elf`
([Hackage](https://hackage.haskell.org/package/elf)) и `data-elf`
([Hackage](https://hackage.haskell.org/package/data-elf)).
Эти библиотеки работают только с заголовками и элементами таблиц и не дают возможности
сгенерировать объектный файл.

Я написал библиотеку для работы с файлами формата ELF на Хаскеле
([GitHub](https://github.com/aleksey-makarov/melf),
 [Hackage](https://hackage.haskell.org/package/melf))
и хочу показать как её использовать.

## Внутреннее устройство ELF

Первые несколько байт файла занимает заголовок ELF.
В нём, в частности, указано, где в файле расположены таблица секций и таблица сегментов.

Сегменты и секции -- непрерывные участки файла.
Сегменты описывают, что нужно поместить в память при загрузки программы,
а секции я бы описал как неделимые результаты работы
компилятора.
В секциях размещены исполняемый код, таблицы символов, инициализированные данные.

Файл формата ELF можно представлять как список деревьев.
Деревьями могуть быть секции, сегменты, заголовок файла, таблица секций, таблица сегментов.
Поддеревья могут содержаться только в сегментах.
Узлы дерева выравниваются в файле, например, сегменты обычно выравниваются на размер страницы,
а секции с данными -- на размер слова.
Вполне валидным может быть файл, где сегмент содержит данные, не размеченные как какая-либо секция.

## Базовый уровень

В модуле
[`Data.Elf.Headers`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf-Headers.html)
реализованы разбор и сериализация заголовка файла ELF и
элементов таблиц секций и сегментов.  Для различения 64- и 32-битных структур 
определён тип
[`ElfClass`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf-Headers.html#t:ElfClass)

``` Haskell
data ElfClass
    = ELFCLASS32 -- ^ 32-bit ELF format
    | ELFCLASS64 -- ^ 64-bit ELF format
    deriving (Eq, Show)
```

Некоторые поля заголовка и элементов таблиц секций и сегментов имеют разную ширину в битах, зависящую от
`ElfClass`, поэтому нужен тип
[`WordXX a`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf-Headers.html#t:WordXX),
который был позаимствован из пакета `data-elf`:

``` Haskell
-- | @IsElfClass a@ is defined for each constructor of `ElfClass`.
--   It defines @WordXX a@, which is `Word32` for `ELFCLASS32`
--   and `Word64` for `ELFCLASS64`.
type IsElfClass :: ElfClass -> Constraint
class ( SingI c
      , Typeable c
      , Typeable (WordXX c)
      , Data (WordXX c)
      , Show (WordXX c)
      , Read (WordXX c)
      , Eq (WordXX c)
      , Ord (WordXX c)
      , Bounded (WordXX c)
      , Enum (WordXX c)
      , Num (WordXX c)
      , Integral (WordXX c)
      , Real (WordXX c)
      , Bits (WordXX c)
      , FiniteBits (WordXX c)
      , Binary (Be (WordXX c))
      , Binary (Le (WordXX c))
      ) => IsElfClass c where
    type WordXX c = r | r -> c

instance IsElfClass 'ELFCLASS32 where
    type WordXX 'ELFCLASS32 = Word32

instance IsElfClass 'ELFCLASS64 where
    type WordXX 'ELFCLASS64 = Word64
```

Заголовок файла ELF представлен с помощью типа
[`HeaderXX a`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf-Headers.html#t:HeaderXX):

``` Haskell
-- | Parsed ELF header
type HeaderXX :: ElfClass -> Type
data HeaderXX c =
    HeaderXX
        { hData       :: ElfData    -- ^ Data encoding (big- or little-endian)
        , hOSABI      :: ElfOSABI   -- ^ OS/ABI identification
        , hABIVersion :: Word8      -- ^ ABI version
        , hType       :: ElfType    -- ^ Object file type
        , hMachine    :: ElfMachine -- ^ Machine type
        , hEntry      :: WordXX c   -- ^ Entry point address
        , hPhOff      :: WordXX c   -- ^ Program header offset
        , hShOff      :: WordXX c   -- ^ Section header offset
        , hFlags      :: Word32     -- ^ Processor-specific flags
        , hPhEntSize  :: Word16     -- ^ Size of program header entry
        , hPhNum      :: Word16     -- ^ Number of program header entries
        , hShEntSize  :: Word16     -- ^ Size of section header entry
        , hShNum      :: Word16     -- ^ Number of section header entries
        , hShStrNdx   :: ElfSectionIndex -- ^ Section name string table index
        }
```

Но это разные типы для 64- и 32-битных форматов.
Для однообразной работы с форматами с разной шириной слова определён тип
[`Header`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf-Headers.html#t:Header):

``` Haskell
-- | Sigma type where `ElfClass` defines the type of `HeaderXX`
type Header = Sigma ElfClass (TyCon1 HeaderXX)
```

`Header` это пара, первый элемент которой -- объект типа `ElfClass`, определяющий ширину слова,
второй -- `HeaderXX`, параметризованный первым элементом ($\Sigma$-тип из языков с зависимыми типами).
Для симуляции $\Sigma$-типов использована библиотека `singletons`
([Hackage](https://hackage.haskell.org/package/singletons),
 ["Introduction to singletons"](https://blog.jle.im/entry/introduction-to-singletons-1.html)).

Для `Header` определён экземпляр класса
[`Binary`](https://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary.html#t:Binary).

Таким образом, имея ленивую строку байт, содержащую достаточно длинный начальный отрезок файла ELF,
можно получить заголовок этого файла, например, следующей функцией:

``` Haskell
withHeader ::                     BSL.ByteString ->
    (forall a . IsElfClass a => HeaderXX a -> b) -> Either String b
withHeader bs f =
    case decodeOrFail bs of
        Left (_, _, err) -> Left err
        Right (_, _, (classS :&: hxx) :: Header) ->
            Right $ withElfClass classS f hxx
```

Здесь
[`decodeOrFail`](https://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary.html#v:decodeOrFail) определена в пакете
[`binary`](https://hackage.haskell.org/package/binary), а
[`withElfClass`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf-Headers.html#v:withElfClass)
похожа на
[`withSing`](https://hackage.haskell.org/package/singletons-3.0.1/docs/Data-Singletons.html#v:withSing):

``` Haskell
-- | Convenience function for creating a
-- context with an implicit ElfClass available.
withElfClass :: Sing c -> (IsElfClass c => a) -> a
withElfClass SELFCLASS64 x = x
withElfClass SELFCLASS32 x = x
```

В `Data.Elf.Headers` определены также типы
[`SectionXX`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf-Headers.html#t:SectionXX),
[`SegmentXX`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf-Headers.html#t:SegmentXX) и
[`SymbolXX`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf-Headers.html#t:SymbolXX)
для элементов таблиц секций, сегментов и символов.

## Верхний уровень

В модуле
[`Data.Elf`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf.html)
реализованы полные разбор и сериализация файлов формата ELF.
Чтобы разобрать такой файл читаются заголовок ELF, таблицa секций и таблица сегментов и
на основании этой информации создаётся список элементов типа
[`ElfXX`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf.html#t:ElfXX),
отображающий рекурсивную
структуру файла ELF.  Кроме восстановления структуры в процессе разбора, например, по номерам
секций восстанавливаются их имена.  В результате получается объект типа
[`Elf`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf.html#t:Elf):

``` Haskell
-- | `Elf` is a forrest of trees of type `ElfXX`.
-- Trees are composed of `ElfXX` nodes, `ElfSegment` can contain subtrees
newtype ElfList c = ElfList [ElfXX c]

-- | Elf is a sigma type where `ElfClass` defines the type of `ElfList`
type Elf = Sigma ElfClass (TyCon1 ElfList)

-- | Section data may contain a string table.
-- If a section contains a string table with section names, the data
-- for such a section is generated and `esData` should contain `ElfSectionDataStringTable`
data ElfSectionData
    = ElfSectionData BSL.ByteString -- ^ Regular section data
    | ElfSectionDataStringTable     -- ^ Section data will be generated from section names

-- | The type of node that defines Elf structure.
data ElfXX (c :: ElfClass)
    = ElfHeader
        { ehData       :: ElfData    -- ^ Data encoding (big- or little-endian)
        , ehOSABI      :: ElfOSABI   -- ^ OS/ABI identification
        , ehABIVersion :: Word8      -- ^ ABI version
        , ehType       :: ElfType    -- ^ Object file type
        , ehMachine    :: ElfMachine -- ^ Machine type
        , ehEntry      :: WordXX c   -- ^ Entry point address
        , ehFlags      :: Word32     -- ^ Processor-specific flags
        }
    | ElfSectionTable
    | ElfSegmentTable
    | ElfSection
        { esName      :: String         -- ^ Section name (NB: string, not offset in the string table)
        , esType      :: ElfSectionType -- ^ Section type
        , esFlags     :: ElfSectionFlag -- ^ Section attributes
        , esAddr      :: WordXX c       -- ^ Virtual address in memory
        , esAddrAlign :: WordXX c       -- ^ Address alignment boundary
        , esEntSize   :: WordXX c       -- ^ Size of entries, if section has table
        , esN         :: ElfSectionIndex -- ^ Section number
        , esInfo      :: Word32         -- ^ Miscellaneous information
        , esLink      :: Word32         -- ^ Link to other section
        , esData      :: ElfSectionData -- ^ The content of the section
        }
    | ElfSegment
        { epType       :: ElfSegmentType -- ^ Type of segment
        , epFlags      :: ElfSegmentFlag -- ^ Segment attributes
        , epVirtAddr   :: WordXX c       -- ^ Virtual address in memory
        , epPhysAddr   :: WordXX c       -- ^ Physical address
        , epAddMemSize :: WordXX c       -- ^ Add this amount of memory after the section when the section is loaded to memory by execution system.
                                         --   Or, in other words this is how much `pMemSize` is bigger than `pFileSize`
        , epAlign      :: WordXX c       -- ^ Alignment of segment
        , epData       :: [ElfXX c]      -- ^ Content of the segment
        }
    | ElfRawData -- ^ Some ELF files (some executables) don't bother to define
                 -- sections for linking and have just raw data in segments.
        { edData :: BSL.ByteString -- ^ Raw data in ELF file
        }
    | ElfRawAlign -- ^ Align the next data in the ELF file.
                  -- The offset of the next data in the ELF file
                  -- will be the minimal @x@ such that
                  -- @x mod eaAlign == eaOffset mod eaAlign @
        { eaOffset :: WordXX c -- ^ Align value
        , eaAlign  :: WordXX c -- ^ Align module
        }

```

Не каждый объект такого типа может быть сериализован.

  * В конструкторе `ElfSection`
остался номер секции.  Он нужен, так как таблица символов и некоторые другие структуры
ссылаются на секци по их номерам.  Поэтому при построении объекта такого типа нужно убедиться,
что секции пронумерованы корректно, т. е. последовательными целыми от 1 до количества секций.
Секция с номером 0 всегда пустая и она добавляется автоматически.

  * В структуре должен быть единственный `ElfHeader`, он должен быть самым первым непустым
    узлом в дереве.

  * Если есть хотя бы один узел `ElfSection`, то должен присутсвовать в точности один узел `ElfSectionTable`
    и в точности одна секция, поле `esData` которой равно `ElfSectionDataStringTable` (таблица строк для имён секций).

  * Если есть хотя бы один узел с конструктором `ElfSegment`, то должен присутсвовать в точности один узел `ElfSegmentTable`.

Корректно сформированный объект можно сериализовать с помощью функции
[`serializeElf`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf.html#v:serializeElf)
и разобрать с помощью функции
[`parseElf`](https://hackage.haskell.org/package/melf-1.0.0/docs/Data-Elf.html#v:parseElf):

``` Haskell
serializeElf :: MonadThrow m => Elf -> m ByteString
parseElf :: MonadCatch m => ByteString -> m Elf
```

Экземпляр класса `Binary` для `ELF` не определён, так как
[`Put`](https://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary.html#t:Put)
не является экземпляром класса `MonadFail`.

## Ассемблер как EDSL для Хакселя

Для использования в демонстрационных приложениях написан модуль, 
генерирующий машинный код для AArch64
(файл [`AsmAarch64.hs`](https://github.com/aleksey-makarov/melf/blob/v1.0.0/examples/AsmAarch64.hs)).
Сгенерированный код использует системные вызовы чтобы вывести на стандартный вывод "Hello World!" и завершить приложение.
Идея позаимствована из вдохновляющей статьи Стивена Дила "От монад к машинному коду"
([Stephen Diehl "Monads to Machine Code"](https://www.stephendiehl.com/posts/monads_machine_code.html)).
Так же как в статье, используется монада состояния, в нашем случае `CodeState`.

Инициализированные, в частности, константные данные часто размещаются в отдельных секциях.
В нашем случае для константной строки использованы массивы литералов (literal pools).
При этом данные размещаются в той же секции, что и машинный код, в месте,
недоступном потоку исполнения, например, после команды возвращения из подпрограммы.
При этом оказывается легко обращаться к таким данным с помощью команд,
вычисляющих адрес данных с использованием счётчика команд.
Код, обращающийся к таким данным, не требует использования таблицы перемещений.
`CodeState` содержит размер массива литералов, сам массив литералов, массив машинных кодов и массив символов:

``` Haskell
data CodeState = CodeState
    { offsetInPool    :: CodeOffset
    , poolReversed    :: [Builder]
    , codeReversed    :: [InstructionGen]
    , symbolsRefersed :: [(String, RelativeRef)]
    }
```

Для создания меток и ссылок на данные в массиве литералов введён тип `RelativeRef`

``` Haskell
newtype CodeOffset  = CodeOffset  { getCodeOffset  :: Int64 }
    deriving (Eq, Show, Ord, Num, Enum, Real, Integral,
    Bits, FiniteBits)

data RelativeRef = CodeRef CodeOffset
                 | PoolRef CodeOffset
```

Конструктор `CodeRef` используется для ссылки на код (для создания меток):

``` Haskell
label :: MonadState CodeState m => m RelativeRef
label = gets (CodeRef . (* instructionSize)
                      . fromIntegral
                      . P.length
                      . codeReversed)
```

Конструктор `PoolRef` хранит смещение данных от начала массива литералов.

Фактически в массиве машинных кодов хранятся функции для генерации машинного кода
из смещения команды от начала секции и смещения массива литералов (которое будет
известно только после обработки всех ассемблерных команд, так как массив литералов
располагается после кода):

```Haskell
type InstructionGen = CodeOffset ->
                      CodeOffset -> Either String Instruction
```

Для добавления в массив машинных кодов используется функция `emit'`:

```Haskell
emit' :: MonadState CodeState m => InstructionGen -> m ()
emit' g = modify f where
    f CodeState {..} = CodeState { codeReversed = g : codeReversed
                                 , ..
                                 }

emit :: MonadState CodeState m => Instruction -> m ()
emit i = emit' $ \ _ _ -> Right i
```

Каждая встретившаяся ассемблерная команда добавляет в этот массив очередной машинный код,
например:

``` Haskell
-- | C6.2.317 SVC
svc :: MonadState CodeState m => Word16 -> m ()
svc imm = emit $ 0xd4000001 .|. (fromIntegral imm `shift` 5)
```

Как и многие другие команды архитектуры AArch64, команда `mov` может работать с регистрами
как с 64-битными или как с 32-битными значениями.
Для указания разрядности регистров для них используются разные имена:
`x0`, `x1`... -- для 64-битных, `w0`, `w1`... -- для 32-битных.
Регистры определены с помощью [фантомного](https://wiki.haskell.org/Phantom_type) типа:


``` Haskell
data RegisterWidth = X | W

type Register :: RegisterWidth -> Type
newtype Register c = R Word32

x0, x1 :: Register 'X
x0 = R 0
x1 = R 1

w0, w1 :: Register 'W
w0 = R 0
w1 = R 1

-- | C6.2.187 MOV (wide immediate)
mov :: (MonadState CodeState m, SingI w) =>
                              Register w ->
                                  Word16 -> m ()
```

В системе команд AArch64 есть несколько вариантов команды `mov`.
Реализована только команда с непосредственным широким аргументом (wide immediate).

Команда `adr` работает с регистрами только как с 64-битными значениями:

``` Haskell
-- | C6.2.10 ADR
adr :: MonadState CodeState m =>
                  Register 'X ->
                  RelativeRef -> m ()
```

Для добавления данных в массив литералов используется функция `emitPool`:

``` Haskell
emitPool :: MonadState CodeState m =>
                              Word ->
                        ByteString -> m RelativeRef
```

Здесь первый аргумент -- необходимое выравнивание, второй -- данные, которые нужно
разместить в массиве.  Функция вычисляет, сколько нужно добавить байт чтобы выравнять
данные, заносит соответствующую нулевую последовательность байт в массив `poolReversed`,
добавляет в этот же массив данные и корректирует `offsetInPool`.

С помощью этой функции можно, например, реализовать аналог ассемблерной директивы `.ascii`:

``` Haskell
ascii :: MonadState CodeState m => String -> m RelativeRef
ascii s = emitPool 1 $ BSLC.pack s
```

Кроме того, есть массив символов.  Символы создаются из меток:

``` Haskell
exportSymbol :: MonadState CodeState m => String -> RelativeRef -> m ()
exportSymbol s r = modify f where
    f (CodeState {..}) = CodeState { symbolsRefersed = (s, r) : symbolsRefersed
                                   , ..
                                   }
```

Функция `assemble` генерирует код из `CodeState`:

``` Haskell
assemble :: MonadCatch m =>
         ElfSectionIndex ->
   StateT CodeState m () -> m (BSL.ByteString, [ElfSymbolXX 'ELFCLASS64])
```

Первый аргумент -- номер секции, которая будет содержать код,
второй -- программа, которую нужно преобразовать в код.
Функция возвращает пару, первый элемент которой -- содержание
секции ".text", второй -- таблица символов.

Для простоты не реализовано обращение к внешним символам и размещение данных в
отдельных секциях.
Всё это требует реализации таблиц перемещений, с другой стороры, сгенерированный код
получается позиционно-независимым.

Код для вывода "Hello World!" на встроенном в Хаскелл DSL выглядит так
([файл `HelloWordl.hs`](https://github.com/aleksey-makarov/melf/blob/v1.0.0/examples/HelloWorld.hs)):

``` Haskell
{-# LANGUAGE DataKinds #-}

module HelloWorld (helloWorld) where

import Prelude as P

import Control.Monad.Catch
import Control.Monad.State
import Data.Word

import AsmAarch64

msg :: String
msg = "Hello World!\n"

sys_exit, sys_write :: Word16
sys_write = 64
sys_exit = 93

helloWorld :: MonadCatch m => StateT CodeState m ()
helloWorld = do

    start <- label
    exportSymbol "_start" start
    mov x0 1
    helloString <- ascii msg
    adr x1 helloString
    mov x2 $ fromIntegral $ P.length msg
    mov x8 sys_write
    svc 0

    mov x0 0
    mov x8 sys_exit
    svc 0
```
Если нужно сослаться на метку, сформированную ниже по коду, нужно работать в монаде `MonadFix`
и использовать ключевое слово `mdo` вместо `do`
(см. файл [`ForwardLabel.hs`](https://github.com/aleksey-makarov/melf/blob/v1.0.0/examples/ForwardLabel.hs)).

## Генерация объектных файлов

Используем библиотеку `melf` для генерации объектных файлов
([файл `MkObj.hs`](https://github.com/aleksey-makarov/melf/blob/v1.0.0/examples/MkObj.hs)):

``` Haskell
{-# LANGUAGE DataKinds #-}

module MkObj (mkObj) where

import Prelude as P

import Control.Monad.Catch
import Control.Monad.State
import Data.Bits
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers

import AsmAarch64

textSecN, shstrtabSecN, strtabSecN, symtabSecN :: ElfSectionIndex
textSecN     = 1
shstrtabSecN = 2
strtabSecN   = 3
symtabSecN   = 4

mkObj :: MonadCatch m => StateT CodeState m () -> m Elf
mkObj m = do

    (txt, symbolTable) <- assemble textSecN m
    (symbolTableData, stringTableData) <- serializeSymbolTable ELFDATA2LSB symbolTable

    return $ SELFCLASS64 :&: ElfList
        [ ElfHeader
            { ehData       = ELFDATA2LSB
            , ehOSABI      = ELFOSABI_SYSV
            , ehABIVersion = 0
            , ehType       = ET_REL
            , ehMachine    = EM_AARCH64
            , ehEntry      = 0
            , ehFlags      = 0
            }
        , ElfSection
            { esName      = ".text"
            , esType      = SHT_PROGBITS
            , esFlags     = SHF_EXECINSTR .|. SHF_ALLOC
            , esAddr      = 0
            , esAddrAlign = 8
            , esEntSize   = 0
            , esN         = textSecN
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionData txt
            }
        , ElfSection
            { esName      = ".shstrtab"
            , esType      = SHT_STRTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 1
            , esEntSize   = 0
            , esN         = shstrtabSecN
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionDataStringTable
            }
        , ElfSection
            { esName      = ".symtab"
            , esType      = SHT_SYMTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 8
            , esEntSize   = symbolTableEntrySize ELFCLASS64
            , esN         = symtabSecN
            , esLink      = fromIntegral strtabSecN
            , esInfo      = 1
            , esData      = ElfSectionData symbolTableData
            }
        , ElfSection
            { esName      = ".strtab"
            , esType      = SHT_STRTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 1
            , esEntSize   = 0
            , esN         = strtabSecN
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionData stringTableData
            }
        , ElfSectionTable
        ]
```

Сгенерируем с помощью этого модуля объектный файл и попробуем его слинковать:

```
[nix-shell:examples]$ ghci 
GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help
Prelude> :l MkObj.hs HelloWorld.hs
[1 of 3] Compiling AsmAarch64       ( AsmAarch64.hs, interpreted )
[2 of 3] Compiling HelloWorld       ( HelloWorld.hs, interpreted )
[3 of 3] Compiling MkObj            ( MkObj.hs, interpreted )
Ok, three modules loaded.
*MkObj> import MkObj 
*MkObj MkObj> import HelloWorld 
*MkObj MkObj HelloWorld> elf <- mkObj helloWorld
*MkObj MkObj HelloWorld> import Data.Elf
*MkObj MkObj HelloWorld Data.Elf> bs <- serializeElf elf
*MkObj MkObj HelloWorld Data.Elf> import Data.ByteString.Lazy as BSL
*MkObj MkObj HelloWorld Data.Elf BSL> BSL.writeFile "hello.o" bs
*MkObj MkObj HelloWorld Data.Elf BSL> 
Leaving GHCi.

[nix-shell:examples]$ aarch64-unknown-linux-gnu-gcc -nostdlib hello.o -o hello 

[nix-shell:examples]$ 
```

Как видим, линковщик благополучно принял сгенерированный объектный файл.
Попробуем запустить результат:

```
[nix-shell:examples]$ qemu-aarch64 hello
Hello World!

[nix-shell:examples]$ 
```

Работает.

## Генерация исполняемых файлов

Так как сгенерированный код для распечатки "Hello World!" является позиционно-независимым
и не ссылается на другие секции, то его можно без изменений использовать для получения
исполняемого файла
([файл `MkExe.hs`](https://github.com/aleksey-makarov/melf/blob/v1.0.0/examples/MkExe.hs)):

``` Haskell
module MkExe (mkExe) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Bits
import Data.Word
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers

import AsmAarch64

addr :: Word64
addr = 0x400000

mkExe :: MonadCatch m => StateT CodeState m () -> m Elf
mkExe m = do
    (txt, _) <- assemble 1 m
    let
        segment = ElfSegment
            { epType       = PT_LOAD
            , epFlags      = PF_X .|. PF_R
            , epVirtAddr   = addr
            , epPhysAddr   = addr
            , epAddMemSize = 0
            , epAlign      = 0x10000
            , epData       =
                [ ElfHeader
                    { ehData       = ELFDATA2LSB
                    , ehOSABI      = ELFOSABI_SYSV
                    , ehABIVersion = 0
                    , ehType       = ET_EXEC
                    , ehMachine    = EM_AARCH64
                    , ehEntry      = addr + headerSize ELFCLASS64
                    , ehFlags      = 0
                    }
                , ElfRawData
                    { edData = txt
                    }
                , ElfSegmentTable
                ]
            }
    return $ SELFCLASS64 :&: ElfList [ segment ]
```

Попробуем его сгенерировать и запустить:

```
[nix-shell:examples]$ ghci 
GHCi, version 8.10.4: https://www.haskell.org/ghc/  :? for help
Prelude> :l MkExe.hs HelloWorld.hs
[1 of 3] Compiling AsmAarch64       ( AsmAarch64.hs, interpreted )
[2 of 3] Compiling HelloWorld       ( HelloWorld.hs, interpreted )
[3 of 3] Compiling MkExe            ( MkExe.hs, interpreted )
Ok, three modules loaded.
*MkExe> import MkExe
*MkExe MkExe> import HelloWorld
*MkExe MkExe HelloWorld> elf <- mkExe helloWorld 
*MkExe MkExe HelloWorld> import Data.Elf
*MkExe MkExe HelloWorld Data.Elf> bs <- serializeElf elf
*MkExe MkExe HelloWorld Data.Elf> import Data.ByteString.Lazy as BSL
*MkExe MkExe HelloWorld Data.Elf BSL> BSL.writeFile "hellox" bs
*MkExe MkExe HelloWorld Data.Elf BSL> 
Leaving GHCi.

[nix-shell:examples]$ chmod +x hellox

[nix-shell:examples]$ qemu-aarch64 hellox
Hello World!

[nix-shell:examples]$ 
```

Работает.
