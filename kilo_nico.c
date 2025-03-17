/*** includes ***/
#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <time.h>
#include <string.h>

/*** Prototype ***/
void editorSetStatusMessage(const char *format, ...) ;
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/*** defines ***/

#define KILO_VERSION "0.0.1"
#define CTRL_KEY(k) ((k) & 0x1f)
#define ABUF_INIT {NULL, 0}
#define TAB_STOP 8
#define NO_FILENAME "[No Name]"
#define INITIAL_STATUS_MESSAGE "AIDE: Ctrl-S = sauver | Ctrl-Q = quitter | Ctrl-f = Rechercher"
#define ERROR_SAVE_FILE "Erreur lors de la sauvegarde: %s"
#define SUCESS_SAVE_FILE "Sauvegarde réussie !"
#define WARNING_UNSAVE_FILE "ATTENTION: Modifications non sauvées. Ctrl-Q %d fois pour quitter."
#define SAVE_PROMPT "Nommer le fichier: %s (ESC pour annuler)"
#define SAVE_ABORT "Sauvegarde annulée !"
#define SEARCH_PROMPT "Rechercher: %s (ESC/Flèches/Entrer/)"
#define QUIT_TIMES 2
#define DEFAULT_TEXT_COLOR (-1)
#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))
#define NO_FILETYPE "type inconnu"


#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

enum editorKey {
  BACKSPACE = 127,
  ARROW_LEFT = 1000,
  ARROW_RIGHT,
  ARROW_UP ,
  ARROW_DOWN,
  DELETE_KEY,
  HOME_KEY ,
  END_KEY ,
  PAGE_UP ,
  PAGE_DOWN
};

enum editorHighlight {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
};

/*** data ***/
typedef struct erow {
  int index;
  ssize_t size; // Taille de la chaine
  int rsize; // Taille de la chaine qui sera affichée
  char *chars;
  char *render; // Chaine affichée
  int hl_open_comment;
  unsigned char *hl;

} erow;

struct editorConfig {
  int cx, cy; // Position
  int rx; // Position x affichée (en fonction des tabs)
  int rowoffset; // Deplacement dans le fichier
  int coloffset; // Deplacement dans le fichier
  int screenrows; // Nombre de lignes dans l'ecran
  int screencols; // Nombre de colonnes dans l'ecran
  int numrows; // Nombre de ligne
  erow *row;
  int dirtybit;
  char *filename;
  char statusmsg[80];
  time_t statusmsg_time;
  struct editorSyntax *syntax;
  struct termios orig_termios;
};

struct append_buffer {
  char *buffer;
  int len;
};

struct editorSyntax {
  char *filetype;
  char **filematch;
  char **keywords;
  char *singleline_comment_start;
  char *multiline_comment_start;
  char *multiline_comment_end;
  int flags;
};

struct editorConfig E;

/*** filetypes ***/
char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case",
  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  "void|", NULL
};

struct editorSyntax HLDB[] = {
  {
    "c",
    C_HL_extensions,
    C_HL_keywords,
    "//",
    "/*" ,
    "*/" ,
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
};

void die(const char *s) {
  write(STDOUT_FILENO, "\x1b[2J", 4);
  write(STDOUT_FILENO, "\x1b[H", 3);
  perror(s);
  exit(1);
}

void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
}

/*** terminal ***/

void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
    die("tcgetattr");

  atexit(disableRawMode);

  struct termios raw = E.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
    die("tcsetattr");
}

int identifyEscapeCaracterNumber(char seq) {
  switch (seq) {
    case '1': return HOME_KEY;
    case '3': return DELETE_KEY;
    case '4': return END_KEY;
    case '5': return PAGE_UP;
    case '6': return PAGE_DOWN;
    case '7': return HOME_KEY;
    case '8': return END_KEY;
  }
  die("identifyEscapeCaracterNumber: Not implemented yet");
  return 0;
}

int identifyEscapeCaracterLetter(char seq) {
  switch (seq) {
    case 'A': return ARROW_UP;
    case 'B': return ARROW_DOWN;
    case 'C': return ARROW_RIGHT;
    case 'D': return ARROW_LEFT;
    case 'H': return HOME_KEY;
    case 'F': return END_KEY;
  }
  die("identifyEscapeCaracterLetter: Not implemented yet");
  return 0;
}

int editorReadKey() {
  ssize_t nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1) die("read");
  }
  if (c == '\x1b') {
    char seq[3];
    if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
    if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
    if (seq[0] == '[') {
      if (seq[1] >= '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
        if (seq[2] == '~') {
          return identifyEscapeCaracterNumber(seq[1]);
        }
      } else {
        return identifyEscapeCaracterNumber(seq[1]);
      }
    } else if (seq[0] == 'O') {
      switch (seq[1]) {
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
      }
    }
    return '\x1b';
  }
  return c;
}

/*** syntax highlighting ***/
int is_separator(int c) {
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
  row->hl = realloc(row->hl, row->rsize);
  memset(row->hl, HL_NORMAL, row->rsize);

  if (E.syntax == NULL) {
    return;
  }

  char **keywords = E.syntax->keywords;

  char *scs = E.syntax->singleline_comment_start;
  char *mcs = E.syntax->multiline_comment_start;
  char *mce = E.syntax->multiline_comment_end;

  int scs_len = scs ? strlen(scs) : 0;
  int mcs_len = mcs ? strlen(mcs) : 0;
  int mce_len = mce ? strlen(mce) : 0;

  int prev_sep = 1, in_string = 0, i = 0;
  int in_comment = row->index > 0 && E.row[row->index - 1].hl_open_comment;
  while (i < row->rsize) {
    char c = row->render[i];
    unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

    if (scs_len && !in_string && !in_comment) {
      if (!strncmp(&row->render[i], scs, scs_len)) {
        memset(&row->hl[i], HL_COMMENT, row->rsize - i);
        break;
      }
    }

    if (mcs_len && mce_len && !in_string) {
      if (in_comment) {
        row->hl[i] = HL_MLCOMMENT;
        if (!strncmp(&row->render[i], mce, mce_len)) {
          memset(&row->hl[i], HL_MLCOMMENT, mce_len);
          i += mce_len;
          in_comment = 0;
          prev_sep = 1;
          continue;
        }
        i++;
        continue;
      }
      if (!strncmp(&row->render[i], mcs, mcs_len)) {
        memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
        i += mcs_len;
        in_comment = 1;
        continue;
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
      if (in_string) {
        row->hl[i] = HL_STRING;
        if (c == '\\' && i + 1 < row->rsize) {
          row->hl[i + 1] = HL_STRING;
          i += 2;
          continue;
        }
        if (c == in_string) {
          in_string = 0;
        }
        i++;
        prev_sep = 1;
        continue;
      }
      if (c == '"' || c == '\'') {
        in_string = c;
        row->hl[i] = HL_STRING;
        i++;
        continue;
      }
    }
    if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
      if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
          (c == '.' && prev_hl == HL_NUMBER)) { // TODO: avec virgule aussi?
        row->hl[i] = HL_NUMBER;
        i++;
        prev_sep = 0;
        continue;
      }
    }

    if (prev_sep) {
      int j = 0;
      for (j = 0; keywords[j]; j++) {
        int klen = strlen(keywords[j]);
        int kw2 = keywords[j][klen - 1] == '|';
        if (kw2) klen--;
        if (!strncmp(&row->render[i], keywords[j], klen) &&
            is_separator(row->render[i + klen])) {
          memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
          i += klen;
          break;
            }
      }
      if (keywords[j] != NULL) {
        prev_sep = 0;
        continue;
      }
    }

    prev_sep = is_separator(c);
    i++;
  }
  int changed = row->hl_open_comment != in_comment;
  row->hl_open_comment = in_comment;
  if (changed && row->index + 1 < E.numrows) {
    editorUpdateSyntax(&E.row[row->index + 1]);
  }
}

int editorSyntaxToColor(int hl) {
  switch (hl) {
    // 30 noir, 31 rouge, 32 vert, 33 Orange, 34 bleu
    // 35 magenta, 36 cyan, 37 gris pale, 39 default
    case HL_COMMENT:
    case HL_MLCOMMENT: return 32;
    case HL_KEYWORD1: return 33;
    case HL_KEYWORD2: return 36;
    case HL_STRING: return 35;
    case HL_NUMBER: return 36;
    case HL_MATCH: return 34;
    default: return 37;
  }
}

void editorSelectSyntaxHighlight() {
  E.syntax = NULL;
  if (E.filename == NULL) return;
  char *ext = strrchr(E.filename, '.');
  for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
    struct editorSyntax *s = &HLDB[j];
    unsigned int i = 0;
    while (s->filematch[i]) {
      int is_ext = s->filematch[i][0] == '.';
      if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
          (!is_ext && strstr(E.filename, s->filematch[i]))) {
        E.syntax = s;
        for (int filerow = 0; filerow < E.numrows; filerow++) {
          editorUpdateSyntax(&E.row[filerow]);
        }
        return;
          }
      i++;
    }
  }
}

/*** Row Operation ***/
int editorRowCxToRx(erow *row, int cx) {
  int rx = 0;
  for (int j = 0; j < cx; j++) {
    if (row->chars[j] == '\t')
      rx += (TAB_STOP - 1) - (rx % TAB_STOP);
    rx++;
  }
  return rx;
}

int editorRowRxToCx(erow *row, int rx) {
  int cur_rx = 0;
  int cx;
  for (cx = 0; cx < row->size; cx++) {
    if (row->chars[cx] == '\t') {
      cur_rx += (TAB_STOP - 1) - (cur_rx % TAB_STOP);
    }
    cur_rx++;
    if (cur_rx > rx) {
      return cx;
    }
  }
  return cx;
}

void editorUpdateRow(erow *row) {
  int tabs = 0;
  for (int i = 0; i < row->size; i++) {
    if (row->chars[i] == '\t') {
      tabs++;
    }
  }

  free(row->render);
  row->render = malloc(row->size + tabs * (TAB_STOP - 1) + 1);

  int idx = 0;
  for (int j = 0; j < row->size; j++) {
    if (row->chars[j] == '\t') {
      row->render[idx++] = ' ';
      while (idx % TAB_STOP != 0) {
        row->render[idx++] = ' ';
      }
    } else {
      row->render[idx++] = row->chars[j];
    }
  }
  row->render[idx] = '\0';
  row->rsize = idx;

  editorUpdateSyntax(row);
}

void editorInsertRow(int row, char *s, size_t len) {
  if (row < 0 || row > E.numrows) {
    return;
  }
  E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
  memmove(&E.row[row + 1], &E.row[row], sizeof(erow) * (E.numrows - row));
  for (int j = row + 1; j <= E.numrows; j++) {
    E.row[j].index++;
  }

  E.row[row].index = row;
  E.row[row].size = len;
  E.row[row].chars = malloc(len + 1 * sizeof(char));
  memcpy(E.row[row].chars, s, len);
  E.row[row].chars[len] = '\0';

  E.row[row].rsize = 0;
  E.row[row].render = NULL;
  E.row[row].hl = NULL;
  E.row[row].hl_open_comment = 0;
  editorUpdateRow(&E.row[row]);
  ++E.numrows; // Cursed
  E.dirtybit = 1;
}

void editorFreeRow(erow *row) {
  free(row->render);
  free(row->chars);
  free(row->hl);
}
void editorDelRow(int at) {
  if (at < 0 || at >= E.numrows) {
    return;
  }
  editorFreeRow(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
  for (int j = at; j < E.numrows; j++) {
    E.row[j].index--;
  }
  E.numrows--;
  E.dirtybit = 1;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row);
  E.dirtybit = 1;
}


void editorRowInsertChar(erow *row, int at, int c) {
  if (at < 0 || at > row->size) {
    at = row->size;
  }
  row->chars = realloc(row->chars, row->size + 2);
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editorUpdateRow(row);
  E.dirtybit = 1;
}

void editorRowDelChar(erow *row, int at) {
  if (at < 0 || at >= row->size) return;
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
  row->size--;
  editorUpdateRow(row);
  E.dirtybit = 1;
}

/*** editor operations ***/
void editorInsertChar(int c) {
  if (E.cy == E.numrows) {
    editorInsertRow(E.numrows, "", 0);
  }
  editorRowInsertChar(&E.row[E.cy], E.cx, c);
  E.cx++;
}

void editorInsertNewline() {
  if (E.cx == 0) {
    editorInsertRow(E.cy, "", 0);
  } else {
    erow *row = &E.row[E.cy];
    editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
    row = &E.row[E.cy];
    row->size = E.cx;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
  }
  E.cy++;
  E.cx = 0;
}

void editorDelChar() {
  if (E.cy == E.numrows) {
    return;
  }
  if (E.cx == 0 && E.cy == 0) {
    return;
  }
  erow *row = &E.row[E.cy];
  if (E.cx > 0) {
    editorRowDelChar(row, E.cx - 1);
    E.cx--;
  } else {
    E.cx = E.row[E.cy - 1].size;
    editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
    editorDelRow(E.cy);
    E.cy--;
  }
}

/*** file i/o ***/
char *editorRowsToString(int *buflen) {
  int totlen = 0;
  for (int j = 0; j < E.numrows; j++) {
    totlen += E.row[j].size + 1;
  }
  *buflen = totlen;
  char *buf = malloc(totlen);
  char *p = buf;
  for (int j = 0; j < E.numrows; j++) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }
  return buf;
}

void editorOpen(char *filename) {
  free(E.filename);
  E.filename = strdup(filename); // Ajout du filename dans struct
  editorSelectSyntaxHighlight();
  FILE *fp = fopen(filename, "r"); // TODO: peut etre O_CREATE
  if (!fp) {
    die("fopen");
  }

  char *line = NULL;
  size_t linecap = 0;
  ssize_t linelen;
  while ((linelen = getline(&line, &linecap, fp)) != -1) {
    while (linelen > 0 && (line[linelen - 1] == '\n' ||
                           line[linelen - 1] == '\r')) {
      linelen--;
    }
    editorInsertRow(E.numrows, line, linelen);
  }
  free(line);
  fclose(fp);
  E.dirtybit = 0;
}

void editorSave() {
  if (E.filename == NULL) {
    E.filename = editorPrompt(SAVE_PROMPT, NULL);
    if (E.filename == NULL) {
      editorSetStatusMessage(SAVE_ABORT);
      return;
    }
    editorSelectSyntaxHighlight();
  }

  int len;
  char *buf = editorRowsToString(&len);
  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);

  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if (write(fd, buf, len) == len) {
        close(fd);
        free(buf);
        E.dirtybit = 0;
        editorSetStatusMessage(SUCESS_SAVE_FILE);
        return;
      }
    }
    close(fd);
  }
  free(buf);
  editorSetStatusMessage(ERROR_SAVE_FILE, strerror(errno));
}

void abFree(struct append_buffer *ab) {
  free(ab->buffer);
}

/*** search ***/
void editorFindCallback(char *query, int key) {
  static int last_match = -1;
  static int direction = 1;

  static int saved_hl_line;
  static char *saved_hl = NULL;
  if (saved_hl) {
    memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
    free(saved_hl);
    saved_hl = NULL;
  }

  if (key == '\r' || key == '\x1b') {
    last_match = -1;
    direction = 1;
    return;
  }
  if (key == ARROW_RIGHT || key == ARROW_DOWN) {
    direction = 1;
  } else if (key == ARROW_LEFT || key == ARROW_UP) {
    direction = -1;
  } else {
    last_match = -1;
    direction = 1;
  }
  if (last_match == -1) {
    direction = 1;
  }
  int current = last_match;
  for (int i = 0; i < E.numrows; i++) {
    current += direction;
    if (current == -1) {
      current = E.numrows - 1;
    }
    else if (current == E.numrows) {
      current = 0;
    }
    erow *row = &E.row[current];
    char *match = strstr(row->render, query);
    if (match) {
      last_match = current;
      E.cy = current;
      E.cx = editorRowRxToCx(row, match - row->render);
      E.rowoffset = E.numrows;

      saved_hl_line = current;
      saved_hl = malloc(row->rsize);
      memcpy(saved_hl, row->hl, row->rsize);
      memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
      break;
    }
  }
}

void editorFind() {
  int saved_cx = E.cx;
  int saved_cy = E.cy;
  int saved_coloff = E.coloffset;
  int saved_rowoff = E.rowoffset;

  char *query = editorPrompt(SEARCH_PROMPT, editorFindCallback);
  if (query) {
    free(query);
  } else {
    E.cx = saved_cx;
    E.cy = saved_cy;
    E.coloffset = saved_coloff;
    E.rowoffset = saved_rowoff;
  }
}

/*** append buffer ***/
void abAppend(struct append_buffer *ab, const char *s, int len) {
  char *new = realloc(ab->buffer, ab->len + len);
  if (new == NULL) return;
  memcpy(&new[ab->len], s, len);
  ab->buffer = new;
  ab->len += len;
}

/*** output ***/
void editorDrawMessageBar(struct append_buffer *ab) {
  abAppend(ab, "\x1b[K", 3);
  int msglen = strlen(E.statusmsg);
  if (msglen > E.screencols) {
    msglen = E.screencols;
  }
  if (msglen && time(NULL) - E.statusmsg_time < 5) {
    abAppend(ab, E.statusmsg, msglen);
  }
}

void editorSetStatusMessage(const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), format, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

void editorDrawStatusBar(struct append_buffer *ab) {
  abAppend(ab, "\x1b[7m", 4);
  char status[80], rstatus[80];
  int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
    E.filename ? E.filename : NO_FILENAME, E.numrows,
    E.dirtybit ? "(modified)" : "");
  int rlen = snprintf(rstatus, sizeof(rstatus), "%s %d/%d",
    E.syntax ? E.syntax->filetype : NO_FILETYPE, E.cy + 1, E.numrows);
  if (len > E.screencols) {
    len = E.screencols;
  }
  abAppend(ab, status, len);
  while (len < E.screencols) {
    if (E.screencols - len == rlen) {
      abAppend(ab, rstatus, rlen);
      break;
    }
    abAppend(ab, " ", 1);
    len++;
  }
  abAppend(ab, "\x1b[m", 3);
  abAppend(ab, "\r\n", 2);
}

void editorScroll() {
  E.rx = 0;
  if (E.cy < E.numrows) {
    E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
  }
  if (E.cy < E.rowoffset) {
    E.rowoffset = E.cy;
  }
  if (E.cy >= E.rowoffset + E.screenrows) {
    E.rowoffset = E.cy - E.screenrows + 1;
  }
  if (E.rx < E.coloffset) {
    E.coloffset = E.rx;
  }
  if (E.rx >= E.coloffset + E.screencols) {
    E.coloffset = E.rx - E.screencols + 1;
  }
}

void editorDrawRows(struct append_buffer *ab) {
  int y;
  for (y = 0; y < E.screenrows; y++) {
    int filerow = y + E.rowoffset;
    if (filerow >= E.numrows) {
      if (E.numrows == 0 && y == E.screenrows / 3) {
        char welcome[80];
        int welcomelen = snprintf(welcome, sizeof(welcome),
          "Nico editor -- version %s", KILO_VERSION);
        if (welcomelen > E.screencols) welcomelen = E.screencols;
        int padding = (E.screencols - welcomelen) / 2;
        if (padding) {
          abAppend(ab, "~", 1);
          padding--;
        }
        while (padding--) abAppend(ab, " ", 1);
        abAppend(ab, welcome, welcomelen);
      } else {
        abAppend(ab, "~", 1);
      }
    } else {
      int len = E.row[filerow].rsize - E.coloffset;
      if (len < 0) {
        len = 0;
      }
      if (len > E.screencols) {
        len = E.screencols;
      }
      char *c = &E.row[filerow].render[E.coloffset];
      unsigned char *hl = &E.row[filerow].hl[E.coloffset];
      int current_color = -1;
      for (int i = 0; i < len; i++) {
        if (iscntrl(c[i])) {
          char sym = (c[i] <= 26) ? '@' + c[i] : '?';
          abAppend(ab, "\x1b[7m", 4);
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3);
          if (current_color != -1) {
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
            abAppend(ab, buf, clen);
          }
        } else if (hl[i] == HL_NORMAL) {
          if (current_color != DEFAULT_TEXT_COLOR) {
            abAppend(ab, "\x1b[39m", 5);
            current_color = DEFAULT_TEXT_COLOR;
          }
          abAppend(ab, &c[i], 1);
        } else {
          int color = editorSyntaxToColor(hl[i]);
          if (color != current_color) {
            current_color = color;
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
            abAppend(ab, buf, clen);
          }
          abAppend(ab, &c[i], 1);
        }
      }
      abAppend(ab, "\x1b[39m", 5);
    }
    abAppend(ab, "\x1b[K", 3);
    abAppend(ab, "\r\n", 2);
  }
}

void editorRefreshScreen() {
  editorScroll();

  struct append_buffer ab = ABUF_INIT;
  abAppend(&ab, "\x1b[?25l", 6); // Cacher le curseur
  abAppend(&ab, "\x1b[H", 3);
  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  char buf[32];
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoffset) + 1,
                                                         (E.rx - E.coloffset) + 1);
  abAppend(&ab, buf, strlen(buf));

  abAppend(&ab, "\x1b[?25h", 6);
  write(STDOUT_FILENO, ab.buffer, ab.len);
  abFree(&ab);
}

int getCursorPosition(int *rows, int *cols) {
  char buf[32];
  unsigned int i = 0;
  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) {
    return -1;
  }
  while (i < sizeof(buf) - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
    if (buf[i] == 'R') break;
    i++;
  }
  buf[i] = '\0';
  if (buf[0] != '\x1b' || buf[1] != '[') {
    return -1;
  }
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) {
    return -1;
  }
  return 0;
}

int getWindowSize(int *rows, int *cols) {
  struct winsize ws;
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12)
      return -1;
    return getCursorPosition(rows, cols);
  } else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

/*** input ***/
char *editorPrompt(char *prompt , void (*callback)(char *, int)) {
  size_t bufsize = 128;
  char *buf = malloc(bufsize);
  size_t buflen = 0;
  buf[0] = '\0';
  while (1) {
    editorSetStatusMessage(prompt, buf);
    editorRefreshScreen();
    int c = editorReadKey();
    if (c ==  DELETE_KEY || c == BACKSPACE || c == CTRL_KEY('h')) {
      if (buflen != 0) {
        buf[--buflen] = '\0';
      }
    } else if (c == '\x1b') {
      editorSetStatusMessage("");
      if (callback) {
        callback(buf, c);
      }
      free(buf);
      return NULL;
    }
    if (c == '\r') {
      if (buflen != 0) {
        editorSetStatusMessage("");
        if (callback) {
          callback(buf, c);
        }
        return buf;
      }
    } else if (!iscntrl(c) && c < 128) {
      if (buflen == bufsize - 1) {
        bufsize *= 2;
        buf = realloc(buf, bufsize);
      }
      buf[buflen++] = c;
      buf[buflen] = '\0';
    }

    if (callback) {
      callback(buf, c);
    }
  }
}

void editorMoveCursor(const int key) {
  erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
  switch (key) {
    case ARROW_LEFT:
      if (E.cx != 0) {
        E.cx--;
      } else if (E.cy > 0) {
        E.cy--;
        E.cx = E.row[E.cy].size;
      }
      break;
    case ARROW_RIGHT:
      if (row && E.cx < row->size) {
        E.cx++;
      } else if (row && E.cx == row->size) {
        E.cy++;
        E.cx = 0;
      }
      break;
    case ARROW_UP:
      if (E.cy != 0) {
        E.cy--;
      }
      break;
    case ARROW_DOWN:
      if (E.cy < E.numrows) {
        E.cy++;
      }
      break;
  }
  int rowlen = row ? row->size : 0;
  if (E.cx > rowlen) {
    E.cx = rowlen;
  }
}

void editorProcessKeypress() {
  static int quit_times = QUIT_TIMES;

  int c = editorReadKey();
  switch (c) {
    case '\r':
      editorInsertNewline();
      break;

    case CTRL_KEY('q'):
      if (E.dirtybit && quit_times > 0) {
        editorSetStatusMessage(WARNING_UNSAVE_FILE, quit_times);
        quit_times--;
        return;
      }
      write(STDOUT_FILENO, "\x1b[2J", 4);
      write(STDOUT_FILENO, "\x1b[H", 3);
      exit(0);

    case BACKSPACE:
    case CTRL_KEY('h'):
    case DELETE_KEY:
      if (c == DELETE_KEY) {
        editorMoveCursor(ARROW_RIGHT);
      }
      editorDelChar();
      break;

    case HOME_KEY:
      E.cx = 0;
      break;
    case END_KEY:
      if (E.cy < E.numrows) {
        E.cx = E.row[E.cy].size;
      }
      break;

    case PAGE_UP:
    case PAGE_DOWN: {
      if (c == PAGE_UP) {
        E.cy = E.rowoffset;
      } else {
        E.cy = E.rowoffset + E.screenrows - 1;
        if (E.cy > E.numrows) E.cy = E.numrows;
      }
      int times = E.screenrows;
      c = c == PAGE_UP ? ARROW_UP : ARROW_DOWN;
      while (times--) {
        editorMoveCursor(c);
      }
      break;
    }

    case CTRL_KEY('s'):
      editorSave();
      break;

    case CTRL_KEY('f'):
      editorFind();
      break;

    case ARROW_UP:
    case ARROW_DOWN:
    case ARROW_LEFT:
    case ARROW_RIGHT:
      editorMoveCursor(c);
      break;

    case CTRL_KEY('l'):
    case '\x1b':
      break;

    default:
      editorInsertChar(c);
      break;
  }
  quit_times = QUIT_TIMES;
}
/*** init ***/

void initEditor() {
  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.numrows = 0;
  E.rowoffset = 0;
  E.coloffset = 0;
  E.row = NULL;
  E.filename = NULL;
  E.dirtybit = 0;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;
  E.syntax = NULL;

  if (getWindowSize(&E.screenrows, &E.screencols) == -1)
    die("getWindowSize");
  E.screenrows -= 2; // On se conserve une ligne pour afficher des infos
}

int main(int argc, char *argv[]) {
  enableRawMode();
  initEditor();
  if (argc == 2) {
    char *fichierInput = argv[1];
    editorOpen(fichierInput);
  }
  editorSetStatusMessage(INITIAL_STATUS_MESSAGE);
  while (1) {
    editorRefreshScreen();
    editorProcessKeypress();

  }

  return 0;
}
// TODO: Retravailer code proprete..
// TODO: Ajouter d'autre langage type (Java, ceux vu en 600c?)
// TODO: Ajout type de fichier + Meilleur Message
// TODO: Afficher nombre ligne a gauche
// TODO: Option auto_indent (selon type de fichier)
// TODO: HARD-WRAP And SOFT WRAP (for txt file)
// TODO Copy-Paste (select text->copier/couper -> coller)
// TODO Menu Option (config file -> Taille tab style)
// TODO: Ajouter option afficherNombreLigne
// TODO: Mode insert?
// TODO CHECK LES MALLOC
