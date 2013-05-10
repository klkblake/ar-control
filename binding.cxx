#include <vector>
#include <opencv2/opencv.hpp>
#include <aruco/aruco.h>
#include <iostream>
#include "binding.h"

#define WIDTH       1280
#define HEIGHT      720
#define MARKER_SIZE 0.071
#define WIN         "Output"
#define FONT_FACE   FONT_HERSHEY_SIMPLEX
#define FONT_SCALE  0.5
#define FONT_THICKNESS 1.5

#define KEY_ESCAPE 27
#define KEY_LEFT   65361
#define KEY_UP     65362
#define KEY_RIGHT  65363
#define KEY_DOWN   65364

using namespace std;
using namespace cv;
using namespace aruco;

int width, height;
VideoCapture *cap;
Mat input, img, overlay;
CameraParameters params;
MarkerDetector detector;
vector<Marker> markers;
Point position;
int *marker_ids;
size_t marker_ids_len = -1;

void blend(cv::Mat &dst, cv::Mat &src){
	double alpha = 0;
	for (int y = 0; y < dst.rows; y++){
		uchar *pdst = dst.ptr<uchar>(y);
		uchar *psrc = src.ptr<uchar>(y);
		for (int x = 0; x < dst.cols; x++){
			double alpha = psrc[x*4+3] / 255.0;
			for (int ch = 0; ch < 3; ch++) {
				pdst[x*3+ch] = psrc[x*4+ch]*alpha + pdst[x*3+ch]*(1-alpha);
			}
		}
	}
}

extern "C" {
	int initialize() {
		cap = new VideoCapture(0);
		cap->set(CV_CAP_PROP_FRAME_WIDTH, WIDTH);
		cap->set(CV_CAP_PROP_FRAME_HEIGHT, HEIGHT);
		cap->set(CV_CAP_PROP_FOURCC, CV_FOURCC('M', 'P', 'E', 'G'));
		if (!cap->isOpened()) {
			return false;
		}

		cap->read(input);
		params.readFromXMLFile("camera.yml");
		params.resize(input.size());
		undistort(input, img, params.getCamMatrix(), params.getDistor());
		overlay = Mat::zeros(input.size(), CV_8UC4);

		namedWindow(WIN);

		width = input.size().width;
		height = input.size().height;
		return true;
	}

	int getWidth() {
		return width;
	}

	int getHeight() {
		return height;
	}

	void nextFrame() {
		blend(img, overlay);
		imshow(WIN, img);
		cap->read(input);
		undistort(input, img, params.getCamMatrix(), params.getDistor());
		overlay = Mat::zeros(input.size(), CV_8UC4);
		detector.detect(input, markers, params, MARKER_SIZE);

		if (marker_ids_len != markers.size()) {
			delete[] marker_ids;
			marker_ids = new int[markers.size()];
			marker_ids_len = markers.size();
		}
		for (int i = 0; i < markers.size(); i++) {
			marker_ids[i] = markers[i].getId();
		}
	}

	int *getMarkers() {
		return marker_ids;
	}

	size_t getMarkersLen() {
		return marker_ids_len;
	}

	Point *getPosition(int id) {
		for (int i = 0; i < markers.size(); i++) {
			if (markers[i].getId() == id) {
				position = markers[i].getCenter();
				return &position;
			}
		}
		fprintf(stderr, "Attempted to retrieve the position of non-existant marker %d\n", id);
		position = Point(0, 0);
		return &position;
	}

	int getKey() {
		return waitKey(10);
	}

	void drawMarker(int id, Color *color) {
		for (int i = 0; i < markers.size(); i++) {
			if (markers[i].getId() == id) {
				markers[i].draw(img, Scalar(color->b, color->g, color->r), 2);
			}
		}
	}

	void drawChevron(Point poly[4], Color *color) {
		fillConvexPoly(overlay, poly, 4, Scalar(color->b, color->g, color->r, 128));
	}

	void drawRect(Point rect[2], Color *color) {
		rectangle(overlay, rect[0], rect[1], Scalar(color->b, color->g, color->r, 128), CV_FILLED);
	}

	void drawText(Point *pos, char *text, Color *color) {
		Size sz = getTextSize(text, FONT_FACE, FONT_SCALE, FONT_THICKNESS, NULL);
		pos->x -= sz.width / 2;
		pos->y += sz.height / 2;
		putText(overlay, text, *pos, FONT_FACE, FONT_SCALE, Scalar(color->b, color->g, color->r, 128), FONT_THICKNESS);
	}
}
