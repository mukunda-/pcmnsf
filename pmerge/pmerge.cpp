#include <stdio.h>

FILE *fout;

typedef enum {
	SRC_DRIVER,
	SRC_DATA
} source_file;

typedef struct t_data_entry {
	source_file sf;
	int start; // inclusive
	int end; // inclusive
} data_entry;

const data_entry datalist[] = {
	{ SRC_DRIVER,	0x0000, 0x007f }, // nsf header
	{ SRC_DATA,		0x0080, 0x0282 }, // music header
	{ SRC_DRIVER,	0x0283, 0x187f }, // program data
	{ SRC_DATA,		0x1880, -1 }, // music data
};

void write_byte( unsigned char b ) {
	unsigned char data[1];
	data[0] = b;
	fwrite( &data, 1, 1, fout );
}

void write_word( unsigned short w ) {
	write_byte( (unsigned char)w );
	write_byte( (unsigned char)(w>>8) );
}

void write_dword( unsigned int w ) {
	write_word( (unsigned short)w );
	write_word( (unsigned short)(w>>16) );
}

void writestr( char *str ) {
	for( int i = 0; str[i]; i++ ) {
		write_byte( str[i] );
	}
}

void writezero( int count ) {
	for( int i = 0; i < count; i++ ) {
		write_byte( 0 );
	}
}

int main( int argc, char *argv[] ) {

	if( argc < 4 ) {
		printf( "usage: pmerge [datafile] [driver] [output]\n" );
		return argc > 1 ? -1 : 0;
	}

	FILE *fdata = fopen( argv[1], "rb" );
	FILE *fdrv = fopen( argv[2], "rb" );

	int datasize;
	int drvsize;
	fseek( fdata, 0, SEEK_END );
	datasize = ftell( fdata );
	fseek( fdata, 0, SEEK_SET );
	fseek( fdrv, 0, SEEK_END );
	drvsize = ftell( fdrv );
	fseek( fdrv, 0, SEEK_SET );

	unsigned char *ppppdata = new unsigned char[ datasize ];
	unsigned char *drvdata = new unsigned char[ drvsize ];

	fread( ppppdata, 1, datasize, fdata );
	fread( drvdata, 1, drvsize, fdrv );

	fclose( fdata );
	fclose( fdrv );

	fout = fopen( argv[3], "wb" );

	// write NSF header
	const data_entry *de;
	for( de = datalist; de; de++ ) {
		if( de->sf == SRC_DRIVER ) {
			int end = (de->end == -1) ? (drvsize-1) : de->end;
			for( int i = de->start; i <= end; i++ )
				write_byte( drvdata[i] );
		} else {
			int end = (de->end == -1) ? (datasize-1) : de->end;
			for( int i = de->start; i <= end; i++ )
				write_byte( ppppdata[i] );
		}
		if( de->end == -1 ) break;
	}

	fclose( fout );
	
	delete[] ppppdata;
	delete[] drvdata;

	return 0;
}
