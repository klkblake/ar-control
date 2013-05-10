
#ifndef BINDING_H
#define BINDING_H

extern "C" {
	enum Side {
		Right,
		Up,
		Left,
		Down,
	};

	typedef struct {
		unsigned char r;
		unsigned char g;
		unsigned char b;
	} Color;

	int initialize();
	int getWidth();
	int getHeight();
	void nextFrame();
	int *getMarkers();
	size_t getMarkersLen();
	cv::Point *getPosition(int id);
	int getKey();
	void drawMarker(int id, Color *color);
	void drawChevron(cv::Point poly[4], Color *color);
	void drawRect(cv::Point rect[2], Color *color);
	void drawText(cv::Point *pos, char *text, Color *color);
}

#endif
