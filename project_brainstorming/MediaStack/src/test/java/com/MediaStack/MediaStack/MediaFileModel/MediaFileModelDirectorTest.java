package com.MediaStack.MediaStack.MediaFileModel;

import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;

import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileModel;
import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileTypeEnum;
import com.MediaStack.MediaStack.entity.model.director.Director;

public class MediaFileModelDirectorTest {

    @Test
    void testConstructVideoFileModel() {
        Director mediaFileDirector = new Director();
        LocalDateTime uploadDate = LocalDateTime.parse("2025-05-30T00:00:00");
        MediaFileModel mediaFile = mediaFileDirector.constructVideoFileModel(12345L, "Sample Media Video", uploadDate, "exampleName/mp4");

        assert mediaFile.getFileType().equals(MediaFileTypeEnum.VIDEO_MP4);
    }

    @Test
    void testConstructImageFileModel() {
        Director mediaFileDirector = new Director();
        LocalDateTime uploadDate = LocalDateTime.parse("2025-05-30T00:00:00");
        MediaFileModel mediaFile = mediaFileDirector.constructImageFileModel(12345L, "Sample Media Image", uploadDate, "exampleName/jpg");

        assert mediaFile.getFileType().equals(MediaFileTypeEnum.IMAGE_JPG);
    }

    @Test
    void testConstructPdfFileModel() {
        Director mediaFileDirector = new Director();
        LocalDateTime uploadDate = LocalDateTime.parse("2025-05-30T00:00:00");
        MediaFileModel mediaFile = mediaFileDirector.constructPdfFileModel(12345L, "Sample Media PDF", uploadDate, "exampleName/pdf");

        assert mediaFile.getFileType().equals(MediaFileTypeEnum.PDF);
    }
}