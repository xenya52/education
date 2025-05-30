package com.MediaStack.MediaStack.MediaFileModel;

import java.time.LocalDateTime;

import org.junit.jupiter.api.Test;

import org.springframework.boot.test.context.SpringBootTest;

import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileModel;
import com.MediaStack.MediaStack.entity.model.mediaFile.MediaFileTypeEnum;
import com.MediaStack.MediaStack.entity.model.builders.MediaFileModelBuilder;

@SpringBootTest
public class MediaFileModelBuilderTest {

    @Test
    void testMediaFileModelBuilder() {
        MediaFileModelBuilder builder = new MediaFileModelBuilder();
        LocalDateTime uploadDate = LocalDateTime.of(2025, 5, 30, 0, 0);

        MediaFileModel mediaFile = builder
                .setId("12345")
                .setName("Sample Media Video")
                .setFileType(MediaFileTypeEnum.VIDEO_MP4)
                .setUploadDate(uploadDate)
                .setPath("exampleName/mp4")
                .build();

        assert mediaFile.getId().equals("12345");
        assert mediaFile.getName().equals("Sample Media Video");
        assert mediaFile.getFileType().equals(MediaFileTypeEnum.VIDEO_MP4);
        assert mediaFile.getUploadDate().equals(uploadDate);
        assert mediaFile.getPath().equals("exampleName/mp4");
    }
}
