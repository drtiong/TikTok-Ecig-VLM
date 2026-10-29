import cv2
import os
from PIL import Image

MAX_FRAMES = 15
TARGET_SIZE = (720, 1280)  # Width: 720, Height: 1280 (9:16 Ratio)


def extract_research_frames(video_path, c_dir=None):
    extracted_images = []
    video = cv2.VideoCapture(video_path)

    if not video.isOpened():
        return []

    fps = video.get(cv2.CAP_PROP_FPS)
    total_frames = int(video.get(cv2.CAP_PROP_FRAME_COUNT))

    if fps <= 0 or total_frames <= 0:
        video.release()
        return []

    # Phase 1: Catch the hooks in the first 3 seconds
    # Timestamps in milliseconds
    early_timestamps = [500, 1500, 2500]
    early_frames_captured = 0

    for ts in early_timestamps:
        video.set(cv2.CAP_PROP_POS_MSEC, ts)
        success, frame = video.read()
        if success:
            frame_resized = cv2.resize(frame, TARGET_SIZE)
            rgb_frame = cv2.cvtColor(frame_resized, cv2.COLOR_BGR2RGB)
            img = Image.fromarray(rgb_frame)
            extracted_images.append(img)
            early_frames_captured += 1

    # Phase 2: Evenly space the remaining frames to reach MAX_FRAMES
    frames_needed = MAX_FRAMES - early_frames_captured

    if frames_needed > 0:
        # Calculate evenly spaced indices across the entire video
        if frames_needed > 1:
            indices = [int(i * (total_frames - 1) / (frames_needed - 1)) for i in range(frames_needed)]
        else:
            indices = [total_frames // 2]

        for count, idx in enumerate(indices):
            video.set(cv2.CAP_PROP_POS_FRAMES, idx)
            success, frame = video.read()
            if success:
                frame_resized = cv2.resize(frame, TARGET_SIZE)

                # Optional: Save to local directory if c_dir is provided
                if c_dir:
                    frame_path = os.path.join(c_dir, f"frame_even_{count:03d}.jpg")
                    cv2.imwrite(frame_path, frame_resized)

                rgb_frame = cv2.cvtColor(frame_resized, cv2.COLOR_BGR2RGB)
                img = Image.fromarray(rgb_frame)
                extracted_images.append(img)

    video.release()

    # If your pipeline requires a specific wrapper (e.g., prepare_image_part), apply it here:
    # return [prepare_image_part(img) for img in extracted_images]

    return extracted_images