package com.MediaStack.MediaStack.entity.model.director;

import java.time.LocalDateTime;

import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileModel;
import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileTypeEnum;

public class Director {
    /**
     * Constructs a video file model with the given parameters.
     *
     * @param id        the unique identifier for the media file
     * @param name      the name of the media file
     * @param uploadDate the date when the media file was uploaded
     * @param path      the path to the media file
     * @return a MediaFileModel representing a video file
     */
    public MediaFileModel constructVideoFileModel(String id, String name, String uploadDate, String path) {
        MediaFileModel mediaFile = new MediaFileModel();
        mediaFile.setId(id);
        mediaFile.setName(name);
        mediaFile.setFileType(MediaFileTypeEnum.VIDEO_MP4);
        mediaFile.setUploadDate(LocalDateTime.parse(uploadDate));
        mediaFile.setPath(path);
        return mediaFile;
    }

    /**
     * Constructs an image file model with the given parameters.
     *
     * @param id        the unique identifier for the media file
     * @param name      the name of the media file
     * @param uploadDate the date when the media file was uploaded
     * @param path      the path to the media file
     * @return a MediaFileModel representing an image file
     */
    public MediaFileModel constructImageFileModel(String id, String name, String uploadDate, String path) {
        MediaFileModel mediaFile = new MediaFileModel();
        mediaFile.setId(id);
        mediaFile.setName(name);
        mediaFile.setFileType(MediaFileTypeEnum.IMAGE_JPG);
        mediaFile.setUploadDate(LocalDateTime.parse(uploadDate));
        mediaFile.setPath(path);
        return mediaFile;
    }

    /**
     * Constructs a PDF file model with the given parameters.
     *
     * @param id        the unique identifier for the media file
     * @param name      the name of the media file
     * @param uploadDate the date when the media file was uploaded
     * @param path      the path to the media file
     * @return a MediaFileModel representing a PDF file
     */
    public MediaFileModel constructPdfFileModel(String id, String name, String uploadDate, String path) {
        MediaFileModel mediaFile = new MediaFileModel();
        mediaFile.setId(id);
        mediaFile.setName(name);
        mediaFile.setFileType(MediaFileTypeEnum.PDF);
        mediaFile.setUploadDate(LocalDateTime.parse(uploadDate));
        mediaFile.setPath(path);
        return mediaFile;
    }
}
