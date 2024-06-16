#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

// defines

#define EDITOR_VER "0.0.1"
#define EDITOR_TAB_STOP 8
#define EDITOR_QUIT_TIMES 1

#define CTRL_KEY(k) ((k) & 0x1f)

enum editor_key {
	BACKSPACE = 127,
	ARROW_LEFT = 1000,
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
	END_KEY,
	PAGE_UP,
	PAGE_DOWN	
};

enum editor_highlight {
	HL_NORMAL = 0,
	HL_COMMENT,
	HL_MLCOMMENT,
	HL_KEYWORD1,
	HL_KEYWORD2,
	HL_STRING,
	HL_NUMBER,
	HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

// data

struct editor_syntax {
	char* filetype;
	char** filematch;
	char** keywords;
	char* single_line_comment_start;
	char* multiline_comment_start;
	char* multiline_comment_end;
	int flags;
};

typedef struct erow {
	int idx;
	int size;
	int rsize;
	char* chars;
	char* render;
	unsigned char* hl;
	int hl_open_comment;
} erow;

struct edConf {
	int cx, cy;
	int rx;
	int rowoff;
	int coloff;
	int screenrows;
	int screencols;
	int numrows;
	erow* row;
	int dirty;
	char* filename;
	char statusmsg[80];
	time_t statusmsg_time;
	struct editor_syntax* syntax;
	struct termios orig_attrs;
};

struct edConf ED;

// filetypes

char* C_HL_exts[] = {".c", ".h", ".cpp", NULL};
char* C_HL_kw[] = {
	"switch", "if", "while", "for", "break", "continue", "return", "else",
  	"struct", "union", "typedef", "static", "enum", "class", "case",
  	"int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  	"void|", NULL
};


struct editor_syntax HLDB[] = { 
	{
		"c",
		C_HL_exts,
		C_HL_kw,
		"//", "/*", "*/",
		HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
	},
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

// prototypes

void setStatusMsg(const char *fmt, ...);
void refreshScreen();
char* prompt( char* prompt, void (*callback)(char*, int) );

// terminal

void die( const char* s ) {
	write( STDOUT_FILENO, "\x1b[2J", 4 );
	write( STDOUT_FILENO, "\x1b[H", 3 );
	perror( s );
	exit( 1 );
}

void disableRawMode() {
	if( tcsetattr( STDIN_FILENO, TCSAFLUSH, &ED.orig_attrs ) == -1 ) {
		die( "tcsetattr" );
	}
}

void enableRawMode() {
	if( tcgetattr( STDIN_FILENO, &ED.orig_attrs ) == -1 ) {
		die( "tcgetattr" );
	}
	atexit( disableRawMode );

	struct termios raw = ED.orig_attrs;
	raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
	raw.c_iflag &= ~(IXON | ICRNL | INPCK | ISTRIP | IXON);
	raw.c_cflag |= (CS8);
	raw.c_oflag &= ~(OPOST);
	raw.c_cc[VMIN] = 0;
	raw.c_cc[VTIME] = 1;

	if( tcsetattr( STDIN_FILENO, TCSAFLUSH, &raw ) == -1 ) {
		die( "tcsetattr" );
	}
}

int readKey() {
	int nread;
	char c;
	while( (nread = read( STDIN_FILENO, &c, 1 )) != 1 ) {
		if( nread == -1 && errno != EAGAIN ) {
			die( "read" );
		}
	}

	if( c == '\x1b' ) {
		char seq[3];
		if( read( STDIN_FILENO, &seq[0], 1 ) != 1 ){
			return c;
		}
		if( read( STDIN_FILENO, &seq[1], 1 ) != 1 ) {
			return c;
		}

		if( seq[0] == '[' ) {
			if( seq[1] >= '0' && seq[1] <= '9' ) {
				if( read( STDIN_FILENO, &seq[2], 1 ) != 1 ) {
					return c;
				}
				if( seq[2] == '~' ) {
					switch( seq[1] ) {
						case '1': return HOME_KEY;
						case '3': return DEL_KEY;
						case '4': return END_KEY;
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;
					}
				}
			}
			else {
				switch( seq[1] ){
					case 'A': return ARROW_UP;
					case 'B': return ARROW_DOWN;
					case 'C': return ARROW_RIGHT;
					case 'D': return ARROW_LEFT;
					case 'H': return HOME_KEY;
					case 'F': return END_KEY;
				}
			}
			
		}
		else if( seq[0] == 'O' ) {
			switch( seq[1] ) {
				case 'H': return HOME_KEY;
				case 'F': return END_KEY;
			}
		}

	}

	return c;
}

int getCursorPosition( int* rows, int* cols ) {
	if( write( STDOUT_FILENO, "\x1b[6n", 4 ) != 4 ) {
		return -1;
	}

	char buf[32];
	unsigned int i = 0;

	while( i < sizeof(buf) - 1 ) {
		if( read( STDIN_FILENO, &buf[i], 1 ) != 1 ) {
			break;
		}
		if( buf[i] == 'R' ) {
			break;
		}
		++i;
	}

	buf[i] = '\0';

	if( buf[0] != '\x1b' || buf[1] != '[' ) {
		return -1;
	}
	if( sscanf( &buf[2], "%d;%d", rows, cols ) != 2 ) {
		return -1;
	}

	return 0;
}

int getWinSize( int* rows, int* cols ) {
	struct winsize ws;

	if( ioctl( STDOUT_FILENO, TIOCGWINSZ, &ws ) == -1 || ws.ws_col == 0 ) {
		if( write( STDOUT_FILENO, "\x1b[999C\x1b[999B", 12 ) != 12 ) {
			return -1;
		}
		return getCursorPosition( rows, cols );
	}

	*cols = ws.ws_col;
	*rows = ws.ws_row;

	return 0;
}

// syntax highlisghting

int isseparator( int c ) {
	return isspace( c ) || c == '\0' || strchr( ",.()+-/*=~%<>[];", c );
}

void updateSyntax( erow* row ) {
	row->hl = realloc( row->hl, row->rsize );
	memset( row->hl, HL_NORMAL, row->rsize );

	if( !ED.syntax ) return;

	char** kw = ED.syntax->keywords;

	char* scs = ED.syntax->single_line_comment_start;
	char* mcs = ED.syntax->multiline_comment_start;
	char* mce = ED.syntax->multiline_comment_end;
	
	int scs_len = scs ? strlen( scs ) : 0;
	int mcs_len = mcs ? strlen( mcs ) : 0;
	int mce_len = mce ? strlen( mce ) : 0;

	int prev_sep = 1;
	int in_string = 0;
	int in_comment = (row->idx > 0 && ED.row[row->idx-1].hl_open_comment);

	int i = 0;
	while( i < row->rsize ) {
		char c = row->render[i];
		unsigned char prev_hl = (i > 0) ? row->hl[i-1] : HL_NORMAL;

		if( scs_len && !in_string && !in_comment ) {
			if( !strncmp( &row->render[i], scs, scs_len ) ) {
				memset( &row->hl[i], HL_COMMENT, row->rsize - i );
				break;
			}
		}

		if( mcs_len && mce_len && !in_string ) {
			if( in_comment ) {
				row->hl[i] = HL_MLCOMMENT;
				if( !strncmp( &row->render[i], mce, mce_len ) ) {
					memset( &row->hl[i], HL_MLCOMMENT, mce_len );
					i += mce_len;
					in_comment = 0;
					prev_sep = 1;
					continue;
				}
				else {
					++i;
					continue;
				}
			}
			else if( !strncmp( &row->render[i], mcs, mcs_len ) ) {
				memset( &row->hl[i], HL_MLCOMMENT, mcs_len );
				i += mcs_len;
				in_comment = 1;
				continue;
			}
		}
		
		if( ED.syntax->flags & HL_HIGHLIGHT_STRINGS ) {
			if( in_string ) {
				row->hl[i] = HL_STRING;
				if( c == '\\' && i + 1 < row->rsize ) {
					row->hl[i+1] = HL_STRING;
					i += 2;
					continue;
				}
				if( c == in_string ) in_string = 0;
				++i;
				prev_sep = 1;
				continue;
			}
			else {
				if( c == '"' || c == '\'' ) {
					in_string = c;
					row->hl[i] = HL_STRING;
					++i;
					continue;
				}
			}
		}

		if( ED.syntax->flags & HL_HIGHLIGHT_NUMBERS ){
			if( (isdigit( c ) && (prev_sep || prev_hl == HL_NUMBER)) ||
			 	(c == '.' && prev_hl == HL_NUMBER) ) {
			row->hl[i] = HL_NUMBER;
			++i;
			prev_sep = 0;
			continue;
			}
		}

		if( prev_sep ) {
			int j;
			for( j = 0; kw[j]; ++j ) {
				int k_len = strlen( kw[j] );
				int kw2 = kw[j][k_len-1] == '|';
				if( kw2 ) --k_len;

				if( !strncmp( &row->render[i], kw[j], k_len ) &&
				 	isseparator( row->render[i+k_len] ) ) {
					memset( &row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, k_len );
					i += k_len;
					break;
				}
			}
			if( kw[j] ) {
				prev_sep = 0;
				continue;
			}
		}

		prev_sep = isseparator(c);
		++i;
	}

	int changed = (row->hl_open_comment != in_comment);
	row->hl_open_comment = in_comment;
	if( changed && row->idx + 1 < ED.numrows ) updateSyntax( &ED.row[row->idx + 1] );

}

int syntaxToColor( int hl ) {
	switch( hl ) {
		case HL_MLCOMMENT:
		case HL_COMMENT: return 36;
		case HL_KEYWORD1: return 33;
		case HL_KEYWORD2: return 32;
		case HL_STRING: return 35;
		case HL_NUMBER: return 31;
		case HL_MATCH: return 34;
		default: return 37;
	}
}

void selectSyntaxHighlight() {
	ED.syntax = NULL;
	if( ED.filename == NULL ) return;

	char* ext = strrchr( ED.filename, '.' );

	for( unsigned int i = 0; i < HLDB_ENTRIES; ++i ) {
		struct editor_syntax *s = &HLDB[i];
		int j = 0;
		while( s->filematch[j] ) {
			int is_ext = (s->filematch[j][0] == '.');
			if( (is_ext && ext && !strcmp( ext, s->filematch[j] )) ||
			 	(!is_ext && strstr( ED.filename, s->filematch[j] )) ) {
				ED.syntax = s;

				for( int filerow = 0; filerow < ED.numrows; ++filerow ) updateSyntax( &ED.row[filerow] );
				return;
			}
			++j;
		}
	}
}

// row operations

int rowCxToRx( erow* row, int cx ) {
	int rx = 0;
	
	for( int i = 0; i < cx; ++i ) {
		if( row->chars[i] == '\t' ) rx += (EDITOR_TAB_STOP - 1) - (rx % EDITOR_TAB_STOP);
		++rx;
	}
	return rx;
}

int rowRxToCx( erow* row, int rx ) {
	int cur_rx = 0;
	int cx;
	for( cx = 0; cx < row->size; ++cx ) {
		if( row->chars[cx] == '\t' ) {
			cur_rx += (EDITOR_TAB_STOP - 1) - (cur_rx % EDITOR_TAB_STOP);
		}
		++cur_rx;

		if( cur_rx > rx ) return cx;
	}

	return cx;
}

void updateRow( erow* row ) {
	int tabs = 0;
	for( int i = 0; i < row->size; ++i ) {
		if( row->chars[i] == '\t' ) ++tabs;
	}
	free( row->render );
	row->render = malloc( row->size + tabs*(EDITOR_TAB_STOP - 1) + 1 );

	int idx = 0;
	for( int i = 0; i < row->size; ++i ) {
		if( row->chars[i] != '\t' ) {
			row->render[idx++] = row->chars[i];
		}
		else {
			row->render[idx++] = ' ';
			while( idx % EDITOR_TAB_STOP != 0 ) row->render[idx++] = ' ';
		}
	}

	row->render[idx] = '\0';
	row->rsize = idx;

	updateSyntax( row );
}

void insertRow( int at, char* s, size_t len ) {
	if( at < 0 || at > ED.numrows ) return;

	ED.row = realloc( ED.row, sizeof(erow) * (ED.numrows + 1) );
	memmove( &ED.row[at+1], &ED.row[at], sizeof(erow) * (ED.numrows - at) );
	for( int i = at + 1; i <= ED.numrows; ++i ) ++ED.row[i].idx;

	ED.row[at].idx = at;

	ED.row[at].size = len;
	ED.row[at].chars = malloc( len + 1 );
	memcpy( ED.row[at].chars, s, len );
	ED.row[at].chars[len] = '\0';

	ED.row[at].rsize = 0;
	ED.row[at].render = NULL;
	ED.row[at].hl = NULL;
	ED.row[at].hl_open_comment = 0;
	updateRow( &ED.row[at] );

	++ED.numrows;
	++ED.dirty;
}

void freeRow( erow* row ) {
	free( row->render );
	free( row->chars );
	free( row->hl );
}

void delRow( int at ) {
	if( at < 0 || at >= ED.numrows ) return;
	freeRow( &ED.row[at] );
	memmove( &ED.row[at], &ED.row[at+1], sizeof(erow) * (ED.numrows - at - 1) );
	for( int i = at; i < ED.numrows - 1; ++i ) --ED.row[i].idx;
	--ED.numrows;
	++ED.dirty;
}

void rowInsertChar( erow* row, int at, int c ) {
	if( at < 0 || at > row->size ) at = row->size;
	row->chars = realloc( row->chars, row->size + 2 );
	memmove( &row->chars[at+1], &row->chars[at], row->size - at + 1 );
	++row->size;
	row->chars[at] = c;
	updateRow( row );
	++ED.dirty;
}

void rowAppendStr( erow* row, char* s, size_t len ) {
	row->chars = realloc( row->chars, row->size + len + 1 );
	memcpy( &row->chars[row->size], s, len );
	row->size += len;
	row->chars[row->size] = '\0';
	updateRow( row );
	++ED.dirty;
}

void rowDelChar( erow* row, int at ) {
	if( at < 0 || at >= row->size ) return;
	memmove( &row->chars[at], &row->chars[at+1], row->size - at );
	--row->size;
	updateRow( row );
	++ED.dirty;
}


// editor operations

void insertChar( int c ) {
	if( ED.cy == ED.numrows ) {
		insertRow( ED.numrows, "", 0 );
	}
	rowInsertChar( &ED.row[ED.cy], ED.cx, c );
	++ED.cx;
}

void insertNewline() {
	if( ED.cx == 0 ) {
		insertRow( ED.cy, "", 0 );
	}
	else {
		erow* row = &ED.row[ED.cy];
		insertRow( ED.cy + 1, &row->chars[ED.cx], row->size - ED.cx );
		row = &ED.row[ED.cy];
		row->size = ED.cx;
		row->chars[row->size] = '\0';
		updateRow( row );
	}
	++ED.cy;
	ED.cx = 0;
}

void delChar() {
	if( ED.cy == ED.numrows ) return;
	if( ED.cx == 0 && ED.cy == 0 ) return;

	erow* row = &ED.row[ED.cy];
	if( ED.cx > 0 ) {
		rowDelChar( row, ED.cx - 1 );
		--ED.cx;
	}
	else {
		ED.cx = ED.row[ED.cy-1].size;
		rowAppendStr( &ED.row[ED.cy-1], row->chars, row->size );
		delRow( ED.cy );
		--ED.cy;
	}
}

// file i/o

char* rowsToString( int* buflen ) {
	int total_len = 0;
	for( int i = 0; i < ED.numrows; ++i ) {
		total_len += ED.row[i].size + 1;
	}
	*buflen = total_len;

	char* buf = malloc( total_len );
	char* p = buf;
	for( int i = 0; i < ED.numrows; ++i ) {
		memcpy( p, ED.row[i].chars, ED.row[i].size );
		p += ED.row[i].size;
		*p = '\n';
		++p;
	}

	return buf;
}

void openFile( const char* file_name ) {
	free( ED.filename );
	ED.filename = strdup(file_name);

	selectSyntaxHighlight();

	FILE* fp = fopen( file_name, "r" );
	if( !fp ) {
		die( "fopen" );
	}

	char* line = NULL;
	size_t line_cap = 0;
	ssize_t line_len;
	while( (line_len = getline( &line, &line_cap, fp )) != -1 ) {
		while( line_len > 0 && (line[line_len-1] == '\n' || line[line_len-1] == '\r') ) {
			line_len--;
		}
		insertRow( ED.numrows, line, line_len );
	}
	free( line );
	fclose( fp );
	ED.dirty = 0;
}

void saveFile() {
	if( ED.filename == NULL ) {
		ED.filename = prompt( "Save as: %s (ESC to cancel)", NULL );
		if( !ED.filename ) {
			setStatusMsg( "Save aborted." );
			return;
		}
		selectSyntaxHighlight();
	}

	int len;
	char* buf = rowsToString( &len );

	int fd = open( ED.filename, O_RDWR | O_CREAT, 0644 );
	if( fd == -1){
		free( buf );
		setStatusMsg( "Can't save, I/O error: %s", strerror( errno ) );
		return;
	}

	if( ftruncate( fd, len ) == -1 ) { 
		close( fd );
		free( buf );
		setStatusMsg( "Can't save, I/O error: %s", strerror( errno ) );
		return;
	}
	if( write( fd, buf, len ) != len ) {
		close( fd );
		free( buf );
		setStatusMsg( "Can't save, I/O error: %s", strerror( errno ) );
		return;
	}
	close( fd );
	free( buf );
	ED.dirty = 0;
	setStatusMsg( "%d bytes written to disk.", len );	
}

// find

void findCallback( char* q, int k ) {
	static int last_match = -1;
	static int direction = 1;

	static int saved_hl_line;
	static char* saved_hl = NULL;

	if( saved_hl ) {
		memcpy( ED.row[saved_hl_line].hl, saved_hl, ED.row[saved_hl_line].rsize );
		free( saved_hl );
		saved_hl = NULL;
	}
	
	if( k == '\r' || k == '\x1b' ) {
	       	last_match = -1;
		direction = 1;
		return;
	}
	else if( k == ARROW_RIGHT || k == ARROW_DOWN ) {
		direction = 1;
	}
	else if( k == ARROW_LEFT || k == ARROW_UP ) {
		direction = -1;
	}
	else {
		last_match = -1;
		direction = 1;
	}

	if( last_match == -1 ) direction = 1;
	int curr = last_match;
	for( int i = 0; i < ED.numrows; ++i ) {
		curr += direction;
		if( curr == -1 ) curr = ED.numrows - 1;
		else if( curr == ED.numrows ) curr = 0;

		erow* row = &ED.row[curr];
		char* match = strstr( row->render, q );
		if( match ) {
			last_match = curr;
			ED.cy = curr;
			ED.cx = rowRxToCx( row, match - row->render );
			ED.rowoff = ED.numrows;

			saved_hl_line = curr;
			saved_hl = malloc( row->rsize );
			memcpy( saved_hl, row->hl, row->rsize );
			memset( &row->hl[match - row->render], HL_MATCH, strlen( q ) );
			break;
		}
	}
}

void find() {
	int old_cx = ED.cx;
	int old_cy = ED.cy;
	int old_coloff = ED.coloff;
	int old_rowoff = ED.rowoff;

	char* query = prompt( "Search: %s (use ESC/Arrows/Enter)", findCallback );
	if( query ) free( query );
	else {
		ED.cx = old_cx;
		ED.cy = old_cy;
		ED.coloff = old_coloff;
		ED.rowoff = old_rowoff;
	}
}

// append buffer

struct abuf {
	char* b;
	int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend( struct abuf *ab, const char* s, int len ) {
	char* new = realloc( ab->b, ab->len + len );

	if( new == NULL ){
		return;
	}

	memcpy( &new[ab->len], s, len );
	ab->b = new;
	ab->len += len;
}

void abFree( struct abuf *ab ) {
	free( ab->b );
}

// output

void scroll() {
	ED.rx = 0;
	if( ED.cy < ED.numrows ) {
			ED.rx = rowCxToRx( &ED.row[ED.cy], ED.cx );
	}

	if( ED.cy < ED.rowoff ) {
		ED.rowoff = ED.cy;
	}
	if( ED.cy >= ED.rowoff + ED.screenrows ) {
		ED.rowoff = ED.cy - ED.screenrows + 1;
	}
	if( ED.rx < ED.coloff ) {
		ED.coloff = ED.rx;
	}
	if( ED.cx >= ED.coloff + ED.screencols ) {
		ED.coloff = ED.rx - ED.screencols + 1;
	}
}

void drawRows( struct abuf* ab ) {
	for( int y = 0; y < ED.screenrows; ++y ) {
		int file_row = y + ED.rowoff;
		if( !ED.numrows && y == ED.screenrows / 3 ) {
			char welcome[80];
			int welcome_len = snprintf( welcome, sizeof(welcome), "Editor -- version %s", EDITOR_VER );
			if( welcome_len > ED.screencols ) {
				welcome_len = ED.screencols;
			}
			int padding = (ED.screencols - welcome_len) / 2;
			if( padding ) {
				abAppend( ab, "~", 1 );
				--padding;
			}
			while( padding-- ) {
				abAppend( ab, " ", 1 );
			}
			abAppend( ab, welcome, welcome_len );
		}
		else if( file_row < ED.numrows ) {
			int len = ED.row[file_row].rsize - ED.coloff;
			if( len < 0 ) len = 0;
			if( len > ED.screencols ) len = ED.screencols;
			char* c = &ED.row[file_row].render[ED.coloff];
			unsigned char* hl = &ED.row[file_row].hl[ED.coloff];
			int curr_col = -1;
			for( int i = 0; i < len; ++i ) {
				if( iscntrl( c[i] ) ) {
					char sym = (c[i] <= 26) ? '@' + c[i] : '?';
					abAppend( ab, "\x1b[7m", 4 );
					abAppend( ab, &sym, 1 );
					abAppend( ab, "\x1b[m", 3 );
					if( curr_col != -1 ) {
						char buf[16];
						int c_len = snprintf( buf, sizeof(buf), "\x1b[%dm", curr_col );
						abAppend( ab, buf, c_len );
					}
				}
				if( hl[i] == HL_NORMAL ) {
					if( curr_col != -1 ) {
						abAppend( ab, "\x1b[39m", 5 );
						curr_col = -1;
					}
					abAppend( ab, &c[i], 1 );
				}
				else {
					int color = syntaxToColor( hl[i] );
					if( color != curr_col ) {
						curr_col = color;
						char buf[16];
						int c_len = snprintf( buf, sizeof(buf), "\x1b[%dm", color );
						abAppend( ab, buf, c_len );
					}
					abAppend( ab, &c[i], 1 );
				}
			}
			abAppend( ab, "\x1b[39m", 5 );
		}
		else {
			abAppend( ab, "~", 1 );
		}
		abAppend( ab, "\x1b[K", 3 );
		abAppend( ab, "\r\n", 2 );
	}
}

void drawStatusBar( struct abuf* ab ) {
	abAppend( ab, "\x1b[7m", 4 );
	char status[80], rstatus[80];
	int len = snprintf( status, sizeof(status), "%.20s - %d lines %s", 
			ED.filename ? ED.filename : "[Untitled]", ED.numrows,
		        ED.dirty ? "(modified)" : "" );
	int rlen = snprintf( rstatus, sizeof(rstatus), "%s | %d/%d", 
		       ED.syntax ? ED.syntax->filetype : "no ft", ED.cy+1, ED.numrows );
	if( len > ED.screencols ) len = ED.screencols;
	abAppend( ab, status, len );
	while( len < ED.screencols ) {
		if( ED.screencols - len == rlen ) {
			abAppend( ab, rstatus, rlen );
			break;
		}
		abAppend( ab, " ", 1 );
		++len;
	}
	abAppend( ab, "\x1b[m", 3 );
	abAppend( ab, "\r\n", 2 );
}

void drawMsgBar( struct abuf* ab ) {
	abAppend( ab, "\x1b[K", 3 );
	int msglen = strlen( ED.statusmsg );
	if( msglen > ED.screencols ) msglen = ED.screencols;
	if( msglen && time(NULL) - ED.statusmsg_time < 5 ) abAppend( ab, ED.statusmsg, msglen );
}


void refreshScreen() {
	scroll();

	struct abuf ab = ABUF_INIT;
	abAppend( &ab, "\x1b[?25l", 6 );
	abAppend( &ab, "\x1b[H", 3 );

	drawRows( &ab );
	drawStatusBar( &ab );
	drawMsgBar( &ab );

	char buf[32];
	snprintf( buf, sizeof(buf), "\x1b[%d;%dH", (ED.cy - ED.rowoff) + 1, (ED.rx - ED.coloff) + 1 );
	abAppend( &ab, buf, strlen(buf) );

	abAppend( &ab, "\x1b[?25h", 6 );

	write( STDOUT_FILENO, ab.b, ab.len );
	abFree( &ab );
}

void setStatusMsg( const char* fmt, ... ) {
	va_list ap;
	va_start( ap, fmt );
	vsnprintf( ED.statusmsg, sizeof(ED.statusmsg), fmt, ap );
	va_end(ap);
	ED.statusmsg_time = time(NULL);
}

// input

char* prompt( char* prompt, void (*callback)(char*, int) ) {
	size_t bufsize = 128;
	char* buf = malloc( bufsize );

	size_t buflen = 0;
	buf[0] = '\0';

	while( 1 ) {
		setStatusMsg( prompt, buf );
		refreshScreen();

		int c = readKey();
		if( c == '\x1b' ) {
			setStatusMsg( "" );
			if( callback ) callback( buf, c );
			free( buf );
			return NULL;
		}
		
		if( c == '\r' ) {
			if( buflen != 0 ) {
				setStatusMsg( "" );
				if( callback ) callback( buf, c );
				return buf;
			}
		}
		else if( c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE ) {
			if( buflen ) {
				buf[--buflen] = '\0';
			}
		}
		else if( !iscntrl(c) && c < 128 ) {
			if( buflen == bufsize - 1 ) {
				bufsize *= 2;
				buf = realloc( buf, bufsize );
			}
			buf[buflen++] = c;
			buf[buflen] = '\0';
		}
		if( callback ) callback( buf, c );
	}
}

void moveCursor( int key ) {
	erow *row = (ED.cy >= ED.numrows) ? NULL : &ED.row[ED.cy];

	switch( key ) {
		case ARROW_LEFT:
			if( ED.cx != 0 ) {
				--ED.cx;
			}
			else if( ED.cy > 0 ) {
				--ED.cy;
				ED.cx = ED.row[ED.cy].size;
			}
			break;
		case ARROW_RIGHT:
			if( row && ED.cx < row->size ) {
				++ED.cx;
			}
			else if( row && ED.cx == row->size ) {
				++ED.cy;
				ED.cx = 0;
			}
			break;
		case ARROW_UP:
			if( ED.cy != 0 ) {
				--ED.cy;
			}
			break;
		case ARROW_DOWN:
			if( ED.cy < ED.numrows ) {
				++ED.cy;
			}
			break;
	}

	row = (ED.cy >= ED.numrows) ? NULL : &ED.row[ED.cy];
	int row_len = row ? row->size : 0;
	if( ED.cx > row_len ) {
		ED.cx = row_len;
	}
}

void processKey() {
	static int quit_times = EDITOR_QUIT_TIMES;
	int c = readKey();

	switch( c ) {
		case '\r':
			insertNewline();
			break;
		case CTRL_KEY('q'):
			if( ED.dirty && quit_times > 0 ) {
				setStatusMsg( "WARNING File has unsaved changes. " 
						"Press Ctrl-Q 1 more time to quit");
				--quit_times;;
				return;
			}
			write( STDOUT_FILENO, "\x1b[2J", 4 );
			write( STDOUT_FILENO, "\x1b[H", 3 );
			exit( 0 );
			break;

		case CTRL_KEY('s'):
			saveFile();
			break;

		case HOME_KEY:
			ED.cx = 0;
			break;
		case END_KEY:
			if( ED.cy < ED.numrows ) ED.cx = ED.row[ED.cy].size;
			break;

		case CTRL_KEY('f'):
			find();
			break;

		case BACKSPACE:
		case CTRL_KEY('h'):
		case DEL_KEY:
			if( c == DEL_KEY ) moveCursor( ARROW_RIGHT );
			delChar();
			break;

		case PAGE_UP:
		case PAGE_DOWN:
			{
			if( c == PAGE_UP ) {
				ED.cy = ED.rowoff;
			}
			else {
				ED.cy = ED.rowoff + ED.screenrows - 1;
				if( ED.cy > ED.numrows ) ED.cy = ED.numrows;
			}

			int times = ED.screenrows;
			while( times-- ) {
				moveCursor( c == PAGE_UP ? ARROW_UP : ARROW_DOWN );
			}
			}
			break;

		case ARROW_UP:
		case ARROW_DOWN:
		case ARROW_LEFT:
		case ARROW_RIGHT:
			moveCursor( c );
			break;
		
		case CTRL_KEY('l'):
		case '\x1b':
		       break;	
		
		default:
			insertChar( c );
			break;
	}

	quit_times = EDITOR_QUIT_TIMES;
}

// init

void init() {
	ED.cx = 0;
	ED.cy = 0;
	ED.rx = 0;
	ED.rowoff = 0;
	ED.coloff = 0;
	ED.numrows = 0;
	ED.row = NULL;
	ED.dirty = 0;
	ED.filename = NULL;
	ED.statusmsg[0] = '\0';
	ED.statusmsg_time = 0;
	ED.syntax = NULL;

	if( getWinSize( &ED.screenrows, &ED.screencols ) == -1 ) {
		die( "getWinSize" );
	}
	ED.screenrows -= 2;
}

int main( int argc, char** argv ) {
	enableRawMode();
	init();
	if( argc >= 2 ) {
		openFile( argv[1] );
	}

	setStatusMsg( "HELP: Ctrl-Q = quit, Ctrl-S = save, Ctrl-F = find." );

	while( 1 ) {
		refreshScreen();
		processKey();
	}	
	return 0;
}
